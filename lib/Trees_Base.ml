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
    val leaf: ?dict:(string StringMap.t) -> ?stem:((edge_t * t) option) -> string -> t
    (* For length, bootstrap, and probability, -1 means undefined.
       When they are specified, length >= 0. and 0. >= bootstrap, probability >= 1. *)
    val edge: ?length:float -> ?bootstrap:float -> ?probability:float ->
              ?dict:(string StringMap.t) -> ?is_ghost:bool -> unit -> edge_t
    val join: ?name:string -> ?dict:(string StringMap.t) -> (edge_t * t) array -> t
    (* Operations on the tree as a whole *)
    val get_name: t -> string
    val get_dict: t -> string StringMap.t
    val get_is_root: t -> bool
    val get_hybrid: t -> hybrid_t option
    val set_name: t -> string -> t
    val set_dict: t -> string StringMap.t -> t
    val set_is_root: t -> bool -> t
    val set_hybrid: t -> hybrid_t option -> t
    (* Operations on edges *)
    val get_edge_values: edge_t -> float * float * float
    val get_edge_length: edge_t -> float
    val get_edge_bootstrap: edge_t -> float
    val get_edge_probability: edge_t -> float
    val get_edge_dict: edge_t -> string StringMap.t
    val get_edge_is_ghost: edge_t -> bool
    val set_edge_values: edge_t -> float * float * float -> edge_t
    val set_edge_length: edge_t -> float -> edge_t
    val set_edge_bootstrap: edge_t -> float -> edge_t
    val set_edge_probability: edge_t -> float -> edge_t
    val set_edge_dict: edge_t -> string StringMap.t -> edge_t
    val set_edge_is_ghost: edge_t -> bool -> edge_t
    (* Operations on nodes (only for flattened representation *)
    val get_node_name: node_t -> string
    val get_node_dict: node_t -> string StringMap.t
    val get_node_is_root: node_t -> bool
    val get_node_hybrid: node_t -> hybrid_t option
    val set_node_name: node_t -> string -> node_t
    val set_node_dict: node_t -> string StringMap.t -> node_t
    val set_node_is_root: node_t -> bool -> node_t
    val set_node_hybrid: node_t -> hybrid_t option -> node_t
    (* Traversals & maps *)
    val dfs_iter: (node_t -> unit) -> (edge_t -> unit) -> t -> unit
    val dfs_iteri: (int -> node_t -> unit) -> (int -> edge_t -> unit) -> t -> unit
    val dfs_map: (node_t -> node_t) -> (edge_t -> edge_t) -> t -> t
    val dfs_mapi: (int -> node_t -> node_t) -> (int -> edge_t -> edge_t) -> t -> t
    (*val dfs_flatten_as_subtrees: t -> t array*)
    (* Flattened representation.
       Each line contains: edge and index of parent node, node, edge and index of children nodes *)
    type flat_t = edge_t * int * node_t * (edge_t * int) array
    val dfs_flatten: t -> flat_t array
    (* Distances *)
    val dijkstra: flat_t array -> int -> Float.Array.t
    val get_min_distance_matrix: ?threads:int -> ?elements_per_step:int -> ?verbose:bool -> t -> Matrix.t
    val get_max_distance_matrix: ?threads:int -> ?elements_per_step:int -> ?verbose:bool -> t -> Matrix.t
    (* I/O *)
    val to_string: ?rich_format:bool -> t -> string
    val array_to_string: ?rich_format:bool -> t array -> string
    val to_file: ?rich_format:bool -> t -> string -> unit
    val array_to_file: ?rich_format:bool -> t array -> string -> unit
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
      node_dict: string StringMap.t
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
      edge_dict: string StringMap.t
    } and hybrid_t =
      | Hybridization of hybrid_id_t
      | GeneTransfer of hybrid_id_t
      | Recombination of hybrid_id_t
    and hybrid_id_t = int
    (* A root leaf node can have a child *)
    let leaf ?(dict = StringMap.empty) ?(stem = None) name =
      match stem with
      | None -> Node ({ node_is_root = false; node_hybrid = None; node_name = name; node_dict = dict }, [||])
      | Some stem -> Node ({ node_is_root = true; node_hybrid = None; node_name = name; node_dict = dict }, [| stem |])
    (* For length, bootstrap, and probability, -1. means undefined.
       When they are specified, length >= 0. and 0. >= bootstrap, probability >= 1. *)
    let edge ?(length = -1.) ?(bootstrap = -1.) ?(probability = -1.)
             ?(dict = StringMap.empty) ?(is_ghost = false) () =
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
        edge_dict = dict }
    let join ?(name = "") ?(dict = StringMap.empty) subs =
      Node ({ node_is_root = false; node_hybrid = None; node_name = name; node_dict = dict }, subs)
    (* *)
    let get_edge_values edge = edge.edge_length, edge.edge_bootstrap, edge.edge_probability [@@inline]
    let get_edge_length edge = edge.edge_length [@@inline]
    let get_edge_bootstrap edge = edge.edge_bootstrap [@@inline]
    let get_edge_probability edge = edge.edge_probability [@@inline]
    let get_edge_dict edge = edge.edge_dict [@@inline]
    let get_edge_is_ghost edge = edge.edge_is_ghost [@@inline]
    let set_edge_values edge (length, bootstrap, probability) =
      { edge with edge_length = length; edge_bootstrap = bootstrap; edge_probability = probability } [@@inline]
    let set_edge_length edge length = { edge with edge_length = length } [@@inline]
    let set_edge_bootstrap edge bootstrap = { edge with edge_bootstrap = bootstrap } [@@inline]
    let set_edge_probability edge probability = { edge with edge_probability = probability } [@@inline]
    let set_edge_dict edge dict = { edge with edge_dict = dict } [@@inline]
    let set_edge_is_ghost edge is_ghost = { edge with edge_is_ghost = is_ghost } [@@inline]
    (* *)
    let get_node_name node = node.node_name [@@inline]
    let get_node_dict node = node.node_dict [@@inline]
    let get_node_is_root node = node.node_is_root [@@inline]
    let get_node_hybrid node = node.node_hybrid [@@inline]
    let set_node_name node name = { node with node_name = name } [@@inline]
    let set_node_dict node dict = { node with node_dict = dict } [@@inline]
    let set_node_is_root node is_root = { node with node_is_root = is_root } [@@inline]
    let set_node_hybrid node hybrid = { node with node_hybrid = hybrid } [@@inline]
    (* *)
    let get_name (Node (node, _)) = get_node_name node [@@inline]
    let get_dict (Node (node, _)) = get_node_dict node [@@inline]
    let get_is_root (Node (node, _)) = get_node_is_root node [@@inline]
    let get_hybrid (Node (node, _)) = get_node_hybrid node [@@inline]
    let set_name (Node (node, desc)) name = Node (set_node_name node name, desc) [@@inline]
    let set_dict (Node (node, desc)) dict = Node (set_node_dict node dict, desc) [@@inline]
    let set_is_root (Node (node, desc)) is_root = Node (set_node_is_root node is_root, desc) [@@inline]
    let set_hybrid (Node (node, desc)) hybrid = Node (set_node_hybrid node hybrid, desc) [@@inline]
    (* *)
    let dfs_iter f_node f_edge t =
      let rec dfs_iter_rec (Node (node, edges)) =
        f_node node;
        Array.iter
          (fun (edge, subnode) ->
            f_edge edge;
            dfs_iter_rec subnode)
          edges in
      dfs_iter_rec t
    let dfs_iteri f_node f_edge t =
      let node_idx = ref 0 and edge_idx = ref 0 in
      let rec dfs_iteri_rec (Node (node, edges)) =
        f_node Int.( !++ node_idx) node;
        Array.iter
          (fun (edge, subnode) ->
            f_edge Int.( !++ edge_idx) edge;
            dfs_iteri_rec subnode)
          edges in
      dfs_iteri_rec t
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
    (*let dfs_flatten_as_subtrees t =
      let res = Tools.StackArray.create () in
      let rec dfs_rec (Node (_, edges) as t) =
        Tools.StackArray.push res t;
        Array.iter
          (fun (_, subnode) ->
            dfs_rec subnode)
          edges in
      dfs_rec t;
      Tools.StackArray.contents res*)
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
      let queue = ref Queue.empty and res = Float.Array.make l Float.infinity in
      Float.Array.set res n 0.;
      Float.Array.iteri
        (fun i dist ->
          queue := Queue.add (dist, i) !queue)
        res;
      let process_link min_dist edge idx =
        let dist = Float.Array.get res idx in
        let elt = dist, idx in
        if Queue.mem elt !queue then begin
          let d = min_dist +. get_edge_length edge in
          if d < dist then begin
            Float.Array.set res idx d;
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
    let _get_distance_matrix ?(invert = false) ?(threads = 1) ?(elements_per_step = 1000) ?(verbose = false) t =
      let ft = begin
        if invert then
          dfs_map (fun node -> node) (fun edge -> { edge with edge_length = -. edge.edge_length }) t
        else
          t
      end |> dfs_flatten in
      let n_nodes = Array.length ft in
      let data = Array.init n_nodes (fun _ -> Float.Array.create 0)
      and nodes_per_step = max 1 (elements_per_step / n_nodes) and processed_nodes = ref 0 in
      (* Generate nodes to be computed by the parallel process *)
      Processes.Parallel.process_stream_chunkwise
        (fun () ->
          if !processed_nodes < n_nodes then
            let to_do = min nodes_per_step (n_nodes - !processed_nodes) in
            let new_processed_nodes = !processed_nodes + to_do in
            let res = !processed_nodes, new_processed_nodes - 1 in
            processed_nodes := new_processed_nodes;
            res
          else
            raise End_of_file)
        (fun (lo_node, hi_node) ->
          let res = ref [] in
          (* We iterate backwards so as to avoid to have to reverse the list in the end *)
          for n = hi_node downto lo_node do
            let d = dijkstra ft n in
            if invert then
              for i = 0 to Float.Array.length d - 1 do
                let f = Float.Array.get d i in
                Float.Array.set d i begin
                  if f = 0. then
                    0.
                  else
                    -. f
                end
              done;
            d |> List.accum res
          done;
          lo_node, !res)
        (fun (lo_row, rows) ->
          List.iteri
            (fun offs_i row_i ->
              data.(lo_row + offs_i) <- row_i;
              if verbose && !processed_nodes mod nodes_per_step = 0 then
                Printf.eprintf "%s\r(%s): Done %d/%d rows%!"
                  String.TermIO.clear __FUNCTION__ !processed_nodes n_nodes;
              incr processed_nodes)
            rows)
        threads;
      if verbose then
        Printf.eprintf "%s\r(%s): Done %d/%d rows.\n%!" String.TermIO.clear __FUNCTION__ !processed_nodes n_nodes;
      (* We need to uniquify names here, or R functions, for instance, will complain *)
      let names = Array.mapi (fun i (_, _, n, _) -> get_node_name n |> Printf.sprintf "%d_%s" i) ft in
      { Matrix.col_names = names;
        row_names = names;
        data = data }
    let get_min_distance_matrix ?(threads = 1) ?(elements_per_step = 1000) ?(verbose = false) t =
      _get_distance_matrix ~invert:false ~threads ~elements_per_step ~verbose t
    let get_max_distance_matrix ?(threads = 1) ?(elements_per_step = 1000) ?(verbose = false) t =
      _get_distance_matrix ~invert:true ~threads ~elements_per_step ~verbose t
    (* *)
    let add_to_buffer ?(rich_format = true) buf (Node ({ node_is_root; _ }, _) as t) =
      let add_hybrid_info buf hy =
        match rich_format, hy with
        | true, Some (Hybridization id) -> Printf.bprintf buf "#H%d" id
        | true, Some (GeneTransfer id) -> Printf.bprintf buf "#LGT%d" id
        | true, Some (Recombination id) -> Printf.bprintf buf "#R%d" id
        | true, None | false, _ -> ()
      and add_dict_info buf dict =
        if rich_format && dict <> StringMap.empty then begin
          Buffer.add_string buf "[&";
          StringMap.iteri
            (fun i k v ->
              if i > 0 then
                Buffer.add_char buf ',';
              Printf.bprintf buf "%s=%s" k v)
            dict;
          Buffer.add_char buf ']'
        end in
      let rec add_to_buffer_rec buf (Node ({ node_hybrid; node_name; node_dict; _ }, edges)) =
        if edges <> [||] then begin
          Buffer.add_char buf '(';
          Array.iteri
            (fun i (edge, (Node (subnode, _) as node)) ->
              if i > 0 then
                Buffer.add_char buf ',';
              if edge.edge_is_ghost then begin
                (* The node becomes terminal *)
                Buffer.add_string buf subnode.node_name;
                add_hybrid_info buf subnode.node_hybrid
              end else
                add_to_buffer_rec buf node;
              begin match edge.edge_length, edge.edge_bootstrap, edge.edge_probability with
              | -1., -1., -1. ->
                if edge.edge_dict <> StringMap.empty then
                  (* Here the only unambiguous way to print out things is by adding an empty length *)
                  Buffer.add_string buf ":0"
              | -1., -1., p -> Printf.bprintf buf ":::%.10g" p
              | -1., b, -1. -> Printf.bprintf buf "::%.10g" b
              | l, -1., -1. -> Printf.bprintf buf ":%.10g" l
              | -1., b, p -> Printf.bprintf buf "::%.10g:%.10g" b p
              | l, -1., p -> Printf.bprintf buf ":%.10g::%.10g" l p
              | l, b, -1. -> Printf.bprintf buf ":%.10g:%.10g" l b
              | l, b, p -> Printf.bprintf buf ":%.10g:%.10g:%.10g" l b p
              end;
              add_dict_info buf edge.edge_dict)
            edges;
          Buffer.add_char buf ')'
        end;
        Buffer.add_string buf node_name;
        add_hybrid_info buf node_hybrid;
        add_dict_info buf node_dict in
      begin match rich_format, node_is_root with
      | true, true -> Buffer.add_string buf "[&R]"
      | true, false -> Buffer.add_string buf "[&U]"
      | false, _ -> ()
      end;
      add_to_buffer_rec buf t;
      Buffer.add_char buf ';';
      buf
    let to_string ?(rich_format = true) t =
      add_to_buffer ~rich_format (Buffer.create 1024) t |> Buffer.contents
    let array_to_string ?(rich_format = true) a =
      let buf = Buffer.create 1024 in
      Array.iter (fun t -> Buffer.add_char (add_to_buffer ~rich_format buf t) '\n') a;
      Buffer.contents buf
    let to_file ?(rich_format = true) t f =
      let f = open_out f and buf = Buffer.create 1024 in
      Buffer.add_char (add_to_buffer ~rich_format buf t) '\n';
      Buffer.output_buffer f buf;
      close_out f
    let array_to_file ?(rich_format = true) a f =
      let f = open_out f and buf = Buffer.create 1024 in
      Array.iter (fun t -> Buffer.add_char (add_to_buffer ~rich_format buf t) '\n') a;
      Buffer.output_buffer f buf;
      close_out f
  end

