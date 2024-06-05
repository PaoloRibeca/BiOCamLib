(*
    Trees_Base.ml -- (c) 2024 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Trees_Base.ml implements tools to represent and process phylogenetic trees.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

open Better

module Newick:
  sig
    type t
    type node_t
    type edge_t
    type hybrid_t =
      | Hybridization of hybrid_id_t
      | GeneTransfer of hybrid_id_t
      | Recombination of hybrid_id_t
    and hybrid_id_t = int
    (* A root leaf node can have a child *)
    val leaf: ?keys:(string StringMap.t) -> ?stem:((edge_t * t) option) -> string -> t
    (* For length, bootstrap, and probability, -1 means undefined.
       When they are specified, length >= 0. and 0. >= bootstrap, probability >= 1. *)
    val edge: ?length:float -> ?bootstrap:float -> ?probability:float ->
              ?keys:(string StringMap.t) -> ?is_ghost:bool -> unit -> edge_t
    val join: ?name:string -> ?keys:(string StringMap.t) -> (edge_t * t) array -> t
    (* Operations on the tree as a whole *)
    val get_name: t -> string
    val get_keys: t -> string StringMap.t
    val get_is_root: t -> bool
    val get_hybrid: t -> hybrid_t option
    val set_name: t -> string -> t
    val set_keys: t -> string StringMap.t -> t
    val set_is_root: t -> bool -> t
    val set_hybrid: t -> hybrid_t option -> t
    (* Operations on edges *)
    val get_edge_values: edge_t -> float * float * float
    val get_edge_length: edge_t -> float
    val get_edge_bootstrap: edge_t -> float
    val get_edge_probability: edge_t -> float
    val get_edge_keys: edge_t -> string StringMap.t
    val get_edge_is_ghost: edge_t -> bool
    val set_edge_values: edge_t -> float * float * float -> edge_t
    val set_edge_length: edge_t -> float -> edge_t
    val set_edge_bootstrap: edge_t -> float -> edge_t
    val set_edge_probability: edge_t -> float -> edge_t
    val set_edge_keys: edge_t -> string StringMap.t -> edge_t
    val set_edge_is_ghost: edge_t -> bool -> edge_t
    (* Operations on nodes (only for flattened representation *)
    val get_node_name: node_t -> string
    val get_node_keys: node_t -> string StringMap.t
    val get_node_is_root: node_t -> bool
    val get_node_hybrid: node_t -> hybrid_t option
    val set_node_name: node_t -> string -> node_t
    val set_node_keys: node_t -> string StringMap.t -> node_t
    val set_node_is_root: node_t -> bool -> node_t
    val set_node_hybrid: node_t -> hybrid_t option -> node_t
    (* Traversals & maps *)
    val dfs: t -> t array
    val dfs_map: (node_t -> node_t) -> (edge_t -> edge_t) -> t -> t
    val dfs_mapi: (int -> node_t -> node_t) -> (int -> edge_t -> edge_t) -> t -> t
    (* Flattened representation.
       Each line contains: edge and index of parent node, node, edge and index of children nodes *)
    type flat_t = edge_t * int * node_t * (edge_t * int) array
    val dfs_flatten: t -> flat_t array
    (* Distances *)
    val dijkstra: flat_t array -> int -> float array
    (* I/O *)
    val to_string: ?rich_format:bool -> t -> string

  end
= struct
    (* The array describes the progeny of the node.
       Only root leaves or hybrid nodes can have progenies of one node.
       In Newick, the root is placed implicitly at the top node,
        so if the top node has a progeny of one, it must be a rooted leaf.
       Hence a node is a leaf if either:
        (1) it is top and has a progeny of one
        (2) it is not top and has a progeny of zero.
       On the other hand, the tree might still be unrooted
        even if Newick must pick up some root node,
        so we require a rooted tree to have an explicitly flagged top node *)
    type t = Node of node_t * (edge_t * t) array
    and node_t = {
      (* In Newick, the root is placed implicitly at the first node.
         We need to explicitly flag the case of an unrooted tree *)
      node_is_root: bool;
      (* Does the node have multiple parents? *)
      node_hybrid: hybrid_t option;
      node_name: string;
      node_keys: string StringMap.t
    } and edge_t = {
      (* In a phylogenetic tree, an edge usually connects one parent to its descendant.
         Hybrid nodes can have multiple parents, but to avoid replication
          only one edge will be allowed to have a substructure (be the "acceptor")
          while all other ones ("ghosts") terminate at the hybrid node,
          effectively turning all the descendant nodes but one into leaves *)
      edge_is_ghost: bool;
      edge_length: float;
      edge_bootstrap: float;
      edge_probability: float;
      edge_keys: string StringMap.t
    } and hybrid_t =
      | Hybridization of hybrid_id_t
      | GeneTransfer of hybrid_id_t
      | Recombination of hybrid_id_t
    and hybrid_id_t = int
    (* A root leaf node can have a child *)
    let leaf ?(keys = StringMap.empty) ?(stem = None) name =
      match stem with
      | None -> Node ({ node_is_root = false; node_hybrid = None; node_name = name; node_keys = keys }, [||])
      | Some stem -> Node ({ node_is_root = true; node_hybrid = None; node_name = name; node_keys = keys }, [| stem |])
    (* For length, bootstrap, and probability, -1. means undefined.
       When they are specified, length >= 0. and 0. >= bootstrap, probability >= 1. *)
    let edge ?(length = -1.) ?(bootstrap = -1.) ?(probability = -1.)
             ?(keys = StringMap.empty) ?(is_ghost = false) () =
      if length < 0. && length <> -1. then
        Printf.sprintf "(%s): Invalid length parameter (found %g)" __FUNCTION__ length |> failwith;
      let check_value what v =
        if (v < 0. && v <> -1.) || v > 1. then
          Printf.sprintf "(%s): Invalid %s parameter (found %g)" __FUNCTION__ what v |> failwith;
        v in
      { edge_is_ghost = is_ghost;
        edge_length = length;
        edge_bootstrap = check_value "bootstrap" bootstrap;
        edge_probability = check_value "probability" probability;
        edge_keys = keys }
    let join ?(name = "") ?(keys = StringMap.empty) subs =
      Node ({ node_is_root = false; node_hybrid = None; node_name = name; node_keys = keys }, subs)
    (* *)
    let get_edge_values edge = edge.edge_length, edge.edge_bootstrap, edge.edge_probability
    let get_edge_length edge = edge.edge_length
    let get_edge_bootstrap edge = edge.edge_bootstrap
    let get_edge_probability edge = edge.edge_probability
    let get_edge_keys edge = edge.edge_keys
    let get_edge_is_ghost edge = edge.edge_is_ghost
    let set_edge_values edge (length, bootstrap, probability) =
      { edge with edge_length = length; edge_bootstrap = bootstrap; edge_probability = probability }
    let set_edge_length edge length = { edge with edge_length = length }
    let set_edge_bootstrap edge bootstrap = { edge with edge_bootstrap = bootstrap }
    let set_edge_probability edge probability = { edge with edge_probability = probability }
    let set_edge_keys edge keys = { edge with edge_keys = keys }
    let set_edge_is_ghost edge is_ghost = { edge with edge_is_ghost = is_ghost }
    (* *)
    let get_node_name node = node.node_name
    let get_node_keys node = node.node_keys
    let get_node_is_root node = node.node_is_root
    let get_node_hybrid node = node.node_hybrid
    let set_node_name node name = { node with node_name = name }
    let set_node_keys node keys = { node with node_keys = keys }
    let set_node_is_root node is_root = { node with node_is_root = is_root }
    let set_node_hybrid node hybrid = { node with node_hybrid = hybrid }
    (* *)
    let get_name (Node (node, _)) = get_node_name node
    let get_keys (Node (node, _)) = get_node_keys node
    let get_is_root (Node (node, _)) = get_node_is_root node
    let get_hybrid (Node (node, _)) = get_node_hybrid node
    let set_name (Node (node, desc)) name = Node (set_node_name node name, desc)
    let set_keys (Node (node, desc)) keys = Node (set_node_keys node keys, desc)
    let set_is_root (Node (node, desc)) is_root = Node (set_node_is_root node is_root, desc)
    let set_hybrid (Node (node, desc)) hybrid = Node (set_node_hybrid node hybrid, desc)
    (* *)

    (*let dfs_iter*)
    let dfs_map f_node f_edge t =
      let rec dfs_map_rec (Node (node, edges)) =
        Node begin
          f_node node,
          Array.map
            (fun (edge, subnode) ->
              f_edge edge, dfs_map_rec subnode)
            edges
        end in
      dfs_map_rec t
    let dfs_mapi f_node f_edge t =
      let node_idx = ref 0 and edge_idx = ref 0 in
      let rec dfs_map_rec (Node (node, edges)) =
        Node begin
          f_node Int.( !++ node_idx) node,
          Array.map
            (fun (edge, subnode) ->
              f_edge Int.( !++ edge_idx) edge, dfs_map_rec subnode)
            edges
        end in
      dfs_map_rec t

    (*let dfs_flatten*)

    let dfs t =
      let res = Tools.StackArray.create () in
      let rec dfs_rec (Node (_, edges) as t) =
        Tools.StackArray.push res t;
        Array.iter
          (fun (_, subnode) ->
            dfs_rec subnode)
          edges in
      dfs_rec t;
      Tools.StackArray.contents res
    type flat_t = edge_t * int * node_t * (edge_t * int) array
    let dfs_flatten t =
      let res = Tools.StackArray.create () in
      let rec dfs_rec (edge_prev, idx_prev) (Node (node, edges)) =
        Tools.StackArray.push res (edge_prev, idx_prev, node, [||]);
        let idx_curr = Tools.StackArray.length res - 1 in
        let v =
          Array.map
            (fun (edge, subnode) ->
              edge, dfs_rec (edge, idx_curr) subnode)
            edges in
        Tools.StackArray.(res.@(idx_curr) <- edge_prev, idx_prev, node, v);
        idx_curr in
      ignore (dfs_rec (edge (), -1) t);
      Tools.StackArray.contents res
    module Queue = Set.Make (MakeComparable (struct type t = float * int end))
    let dijkstra fl n =
      let l = Array.length fl in
      if n < 0 || n >= l then
        Printf.sprintf "(%s): Index %d is out of range" __FUNCTION__ n |> failwith;
      let queue = ref Queue.empty and res = Array.make l Float.infinity in
      res.(n) <- 0.;
      Array.iteri
        (fun i dist ->
          queue := Queue.add (dist, i) !queue)
        res;
      let process_link min_dist edge idx =
        let dist = res.(idx) in
        let elt = dist, idx in
        if Queue.mem elt !queue then begin
          let d = min_dist +. get_edge_length edge in
          if d < dist then begin
            res.(idx) <- d;
            queue := Queue.remove elt !queue |> Queue.add (d, idx)
          end
        end in
      while !queue <> Queue.empty do
        let min_dist, min_idx as min_elt = Queue.min_elt !queue in
        queue := Queue.remove min_elt !queue;
        let prog_edge, prog_idx, _, descs = fl.(min_idx) in
        (* Progenitor node *)
        if prog_idx <> -1 then
          process_link min_dist prog_edge prog_idx;
        (* Descendant nodes *)
        Array.iter
          (fun (desc_edge, desc_idx) ->
            process_link min_dist desc_edge desc_idx)
          descs
      done;
      res
    (* *)
    let to_string ?(rich_format = true) (Node ({ node_is_root; _ }, _) as t) =
      let get_hybrid_info hy =
        match rich_format, hy with
        | true, Some (Hybridization id) -> Printf.sprintf "#H%d" id
        | true, Some (GeneTransfer id) -> Printf.sprintf "#LGT%d" id
        | true, Some (Recombination id) -> Printf.sprintf "#R%d" id
        | true, None | false, _ -> "" in
      let rec to_string_rec (Node ({ node_hybrid; node_name; node_keys; _ }, edges)) =

ignore (node_keys);

        begin if edges <> [||] then
          "(" ^ begin
            Array.fold_left
              (fun res (edge, (Node (subnode, _) as node)) ->
                res ^ begin if res <> "" then "," else "" end ^ begin
                  if edge.edge_is_ghost then
                    (* The node becomes terminal *)
                    subnode.node_name ^ get_hybrid_info subnode.node_hybrid
                  else
                    to_string_rec node
                end ^
                  match edge.edge_length, edge.edge_bootstrap, edge.edge_probability with
                  | -1., -1., -1. -> ""
                  | -1., -1., p -> Printf.sprintf ":::%.10g" p
                  | -1., b, -1. -> Printf.sprintf "::%.10g" b
                  | l, -1., -1. -> Printf.sprintf ":%.10g" l
                  | -1., b, p -> Printf.sprintf "::%.10g:%.10g" b p
                  | l, -1., p -> Printf.sprintf ":%.10g::%.10g" l p
                  | l, b, -1. -> Printf.sprintf ":%.10g:%.10g" l b
                  | l, b, p -> Printf.sprintf ":%.10g:%.10g:%.10g" l b p)
              ""
              edges
          end ^ ")"
        else 
          ""
        end ^ node_name ^ get_hybrid_info node_hybrid in
      begin match rich_format, node_is_root with
      | true, true -> "[&R]"
      | true, false -> "[&U]"
      | false, _ -> ""
      end ^ to_string_rec t ^ ";"
          
  end

