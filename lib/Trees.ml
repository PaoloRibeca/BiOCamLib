(*
    Trees.ml -- (c) 2024 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Trees.ml implements tools to represent and process phylogenetic trees.

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
    type edge_t
    type parents_t =
      | Root
      | Hybridization of hybrid_id_t
      | GeneTransfer of hybrid_id_t
      | Recombination of hybrid_id_t
    and hybrid_id_t = int
    val leaf: ?keys:(string StringMap.t) -> ?stem:((edge_t * t) option) -> string -> t
    (* For length, bootstrap, and probability, -1 means undefined.
       When they are specified, length >= 0. and 0. >= bootstrap, probability >= 1. *)
    val edge: ?length:float -> ?bootstrap:float -> ?probability:float ->
              ?keys:(string StringMap.t) -> ?is_ghost:bool -> unit -> edge_t
    val join: ?name:string -> ?keys:(string StringMap.t) -> (edge_t * t) array -> t
    val set_parents: t -> parents_t option -> t
    val set_edge_is_ghost: edge_t -> bool -> edge_t
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
      (* Does the node have zero or multiple parents?
         In Newick, the root is placed implicitly at the first node.
          We need to explicitly flag the case of an unrooted tree *)
      node_parents: parents_t option;
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
    } and parents_t =
      | Root
      | Hybridization of hybrid_id_t
      | GeneTransfer of hybrid_id_t
      | Recombination of hybrid_id_t
    and hybrid_id_t = int
    (* A root leaf node can have a child *)
    let leaf ?(keys = StringMap.empty) ?(stem = None) name =
      Node ({ node_parents = None; node_name = name; node_keys = keys }, begin
              match stem with
              | None -> [||]
              | Some stem -> [| stem |]
            end)
    (* For length, bootstrap, and probability, -1. means undefined.
       When they are specified, length >= 0. and 0. >= bootstrap, probability >= 1. *)
    let edge ?(length = -1.) ?(bootstrap = -1.) ?(probability = -1.)
             ?(keys = StringMap.empty) ?(is_ghost = false) () =
      { 

        (* TODO: IMPLEMENT CHECKS *)


        edge_is_ghost = is_ghost;
        edge_length = length; edge_bootstrap = bootstrap; edge_probability = probability;
        edge_keys = keys }
    let join ?(name = "") ?(keys = StringMap.empty) subs =
      Node ({ node_parents = None; node_name = name; node_keys = keys }, subs)
    let set_parents (Node (node, desc)) parents = Node ({ node with node_parents = parents }, desc)
    let set_edge_is_ghost edge is_ghost = { edge with edge_is_ghost = is_ghost }
    let to_string ?(rich_format = true) (Node ({ node_parents; _ }, _) as t) =
      let get_parental_info pt =
        match rich_format, pt with
        | true, Some Root | true, None | false, _ -> ""
        | true, Some (Hybridization id) -> Printf.sprintf "#H%d" id
        | true, Some (GeneTransfer id) -> Printf.sprintf "#LGT%d" id
        | true, Some (Recombination id) -> Printf.sprintf "#R%d" id in
      let rec to_string_rec (Node ({ node_parents; node_name; node_keys }, edges)) =

ignore (node_keys);

        begin if edges <> [||] then
          "(" ^ begin
            Array.fold_left
              (fun res (edge, (Node (subnode, _) as node)) ->
                res ^ begin if res <> "" then "," else "" end ^ begin
                  if edge.edge_is_ghost then
                    (* The node becomes terminal *)
                    subnode.node_name ^ get_parental_info subnode.node_parents
                  else
                    to_string_rec node
                end ^
                  match edge.edge_length, edge.edge_bootstrap, edge.edge_probability with
                  | -1., -1., -1. -> ""
                  | -1., -1., p -> Printf.sprintf ":::%g" p
                  | -1., b, -1. -> Printf.sprintf "::%g" b
                  | l, -1., -1. -> Printf.sprintf ":%g" l
                  | -1., b, p -> Printf.sprintf "::%g:%g" b p
                  | l, -1., p -> Printf.sprintf ":%g::%g" l p
                  | l, b, -1. -> Printf.sprintf ":%g:%g" l b
                  | l, b, p -> Printf.sprintf ":%g:%g:%g" l b p)
              ""
              edges
          end ^ ")"
        else 
          ""
        end ^ node_name ^ get_parental_info node_parents in
      begin match rich_format, node_parents with
      | true, Some Root -> "[&R]"
      | true, _ -> "[&U]"
      | false, _ -> ""
      end ^ to_string_rec t ^ ";"
          
  end

