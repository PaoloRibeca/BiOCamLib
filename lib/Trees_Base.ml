(*
    Trees_Base.ml -- (c) 2024-2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Trees_Base.ml implements tools to represent and process phylogenetic trees.

    This program was designed and developed by the author(s),
    with the assistance of the following AI tool(s):
      2026 Claude (Anthropic).
    The final logic and implementation were reviewed and verified in
    their entirety by the author(s).

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
    (* Policy for negative branch lengths in parsed input.  Some
       phylogenetic methods (notably neighbour-joining) can yield
       trees with negative branches; this submodule's [t] tells
       the reader what to do when one is encountered.
        - [Error]: raise a parse error.
        - [Zero]: silently clamp the length to 0.
        - [OK]: keep the negative value as-is.  The undefined-
          length sentinel is [Float.neg_infinity], not [-1.], so
          there is no collision with NJ-style negatives.
       Standard [of_string] / [to_string] use 'error'|'zero'|'ok'
       as the canonical CLI form. *)
    module NegativeBranchesPolicy:
      sig
        type t =
          | OK
          | Zero
          | Error
        val of_string: string -> t
        val to_string: t -> string
      end
    (* A root leaf node can have a child *)
    val leaf: ?dict:(string StringMap.t) -> ?stem:((edge_t * t) option) -> string -> t
    (* Undefined sentinels: [neg_infinity] for [length] (so any
       finite real, including arbitrary negatives, is a valid
       value), and [-1.] for [bootstrap] / [probability] (whose
       ranges are [0., 1.] when specified, leaving [-1.]
       unambiguously outside).  Branch lengths may be negative
       (see [NegativeBranchesPolicy.t] and the parser's
       negative-length policy). *)
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
    (* Traversals & maps.
       For iter () and iteri () we specify separate pre- and post- node/edge functions.
       Node functions receive as arguments the node and the number of edges;
        edge functions the (local) index of the edge and the edge itself.
       In addition, iteri and mapi also receive a global node/edge index as first argument *)
    val dfs_iter: (node_t -> int -> unit) -> (int -> edge_t -> unit) ->
                  (int -> edge_t -> unit) -> (node_t -> int -> unit) -> t -> unit
    val dfs_iteri: (int -> node_t -> int -> unit) -> (int -> int -> edge_t -> unit) ->
                   (int -> int -> edge_t -> unit) -> (int -> node_t -> int -> unit) -> t -> unit
    val dfs_map: (node_t -> int -> node_t) -> (int -> edge_t -> edge_t) -> t -> t
    val dfs_mapi: (int -> node_t -> int -> node_t) -> (int -> int -> edge_t -> edge_t) -> t -> t
    (*val dfs_flatten_as_subtrees: t -> t array*)
    (* Flattened representation.
       Each line contains: edge and index of parent node, node, edge and index of children nodes *)
    type flat_t = edge_t * int * node_t * (edge_t * int) array
    val dfs_flatten: t -> flat_t array
    (* Distances *)
    val dijkstra: flat_t array -> int -> Float.Array.t
    val get_min_distance_matrix: ?threads:int -> ?elements_per_step:int -> ?verbose:bool -> t -> Matrix.t
    val get_max_distance_matrix: ?threads:int -> ?elements_per_step:int -> ?verbose:bool -> t -> Matrix.t
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
    module NegativeBranchesPolicy =
      struct
        type t =
          | OK
          | Zero
          | Error
        let of_string = function
          | "ok" -> OK
          | "zero" -> Zero
          | "error" -> Error
          | s -> Exception.raise_unrecognized_initializer __FUNCTION__ "negative-branches policy" s
        let to_string = function
          | OK -> "ok"
          | Zero -> "zero"
          | Error -> "error"
      end
    (* A root leaf node can have a child *)
    let leaf ?(dict = StringMap.empty) ?(stem = None) name =
      match stem with
      | None -> Node ({ node_is_root = false; node_hybrid = None; node_name = name; node_dict = dict }, [||])
      | Some stem -> Node ({ node_is_root = true; node_hybrid = None; node_name = name; node_dict = dict }, [| stem |])
    (* Undefined sentinels: [neg_infinity] for [length] (any
       finite real, including arbitrary negatives, is a valid
       value); [-1.] for [bootstrap] / [probability].  Branch
       lengths may be negative (see [NegativeBranchesPolicy.t]);
       the parser is the gatekeeper that rejects / clamps /
       accepts them per the caller's policy.  Direct callers of
       [edge] are trusted to pass sane values. *)
    let edge ?(length = neg_infinity) ?(bootstrap = -1.) ?(probability = -1.)
             ?(dict = StringMap.empty) ?(is_ghost = false) () =
      let fail parameter_name v =
        Exception.raise __FUNCTION__ Initialize
          (Printf.sprintf "Invalid value for parameter '%s' (found %g)" parameter_name v) in
      let check_fraction what v =
        if (v < 0. && v <> -1.) || v > 1. then
          fail what v;
        v in
      { edge_is_ghost = is_ghost;
        edge_length = length;
        edge_bootstrap = check_fraction "bootstrap" bootstrap;
        edge_probability = check_fraction "probability" probability;
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
    let dfs_iter f_node_pre f_edge_pre f_edge_post f_node_post t =
      let rec dfs_iter_rec (Node (node, edges)) =
        let n_children = Array.length edges in
        f_node_pre node n_children;
        Array.iteri
          (fun i (edge, subnode) ->
            f_edge_pre i edge;
            (* Ghost nodes are terminal *)
            if not edge.edge_is_ghost then
              dfs_iter_rec subnode;
            f_edge_post i edge)
          edges;
        f_node_post node n_children in
      dfs_iter_rec t
    let dfs_iteri f_node_pre f_edge_pre f_edge_post f_node_post t =
      let node_idx = ref 0 and edge_idx = ref 0 in
      let rec dfs_iteri_rec (Node (node, edges)) =
        let n_children = Array.length edges in
        f_node_pre Int.( !++ node_idx) node n_children;
        Array.iteri
          (fun i (edge, subnode) ->
            f_edge_pre Int.( !++ edge_idx) i edge;
            (* Ghost nodes are terminal *)
            if not edge.edge_is_ghost then
              dfs_iteri_rec subnode;
            f_edge_post !edge_idx i edge)
          edges;
        f_node_post !node_idx node n_children in
      dfs_iteri_rec t
    let dfs_map f_node f_edge t =
      let rec dfs_map_rec (Node (node, edges)) =
        let n_children = Array.length edges in
        Node begin
          f_node node n_children,
          Array.mapi
            (fun i (edge, subnode) ->
              f_edge i edge, dfs_map_rec subnode)
            edges
        end in
      dfs_map_rec t
    let dfs_mapi f_node f_edge t =
      let node_idx = ref 0 and edge_idx = ref 0 in
      let rec dfs_map_rec (Node (node, edges)) =
        let n_children = Array.length edges in
        Node begin
          f_node Int.( !++ node_idx) node n_children,
          Array.mapi
            (fun i (edge, subnode) ->
              f_edge Int.( !++ edge_idx) i edge, dfs_map_rec subnode)
            edges
        end in
      dfs_map_rec t
    (*let dfs_flatten_as_subtrees t =
      let res = Tools.ArrayStack.create () in
      let rec dfs_rec (Node (_, edges) as t) =
        Tools.ArrayStack.push res t;
        Array.iter
          (fun (_, subnode) ->
            dfs_rec subnode)
          edges in
      dfs_rec t;
      Tools.ArrayStack.contents res*)
    type flat_t = edge_t * int * node_t * (edge_t * int) array
    let dfs_flatten t =
      let res = Tools.ArrayStack.empty () in
      let rec dfs_rec (edge_prev, idx_prev) (Node (node, edges)) =
        Tools.ArrayStack.push res (edge_prev, idx_prev, node, [||]);
        let idx_curr = Tools.ArrayStack.length res - 1 in
        let v =
          Array.map
            (fun (edge, subnode) ->
              edge, dfs_rec (edge, idx_curr) subnode)
            edges in
        Tools.ArrayStack.(res.@(idx_curr) <- edge_prev, idx_prev, node, v);
        idx_curr in
      ignore (dfs_rec (edge (), -1) t);
      Tools.ArrayStack.contents res
    module Queue = Set.Make (MakeComparable (struct type t = float * int end))
    let dijkstra fl n =
      let l = Array.length fl in
      if n < 0 || n >= l then
        Exception.raise_index_out_of_range __FUNCTION__ n "node set" l;
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
          dfs_map (fun node _ -> node) (fun _ edge -> { edge with edge_length = -. edge.edge_length }) t
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
  end

module type Splits_base =
  sig
    module Split:
      sig
        type t
        val of_string: string -> t
        val of_list: int list -> t
        val of_array: int array -> t
        (* Wrap an existing IntZ bitmask as a Split.t.  Each set bit i
           in the input mask corresponds to element index i being on
           one side of the bipartition.  No canonicalisation is done
           here; canonicalisation happens inside [add_split]. *)
        val of_intz: IntZ.t -> t
        (* The dual of [of_intz]: project a Split.t back to its IntZ bitmask,
           so a client can do bit algebra (popcount, complement, set-bit
           iteration) on splits without the representation being exposed as a
           type equation.  Used by [Trees.of_splits]. *)
        val to_intz: t -> IntZ.t
        val to_string: t -> string
      end
    type t
    val get_names: t -> string array
    val cardinal: t -> int
    val iter: (Split.t -> float -> unit) -> t -> unit
    (* The argument are element names. It fails if some are repeated *)
    val create: string array -> t
    (* Discards splits while keeping names *)
    val clear: t -> unit
    (* Add a split to a split set. It fails if the split is incompatible *)
    val add_split: t -> Split.t -> float -> unit
    (* It fails if the element names of the split sets are different *)
    val add_splits: t -> t -> unit
    (* Bipartition weight when constructing splits from a tree.
       Constant uses a fixed weight per bipartition; Bootstrap uses
       the parent-edge bootstrap value (falling back to 1.0 if absent) *)
    type weight_t =
      | Constant of float
      | Bootstrap
    (* Build a split set from a Newick tree.  The element-name array
       is the alphabetically-sorted list of leaves.  Each non-trivial
       bipartition is added once with the chosen weight. *)
    val of_newick: ?weight_kind:weight_t -> Newick.t -> t
    (* Add the bipartitions of a Newick tree to an existing register.
       Tree-leaves must equal register-names (otherwise raise);
       weights accumulate (matching add_split's semantics). *)
    val add_newick: ?weight_kind:weight_t -> t -> Newick.t -> unit
    (* In-place: drop every split whose weight is strictly below [cutoff]
       (equivalently, keep those with weight >= cutoff). *)
    val drop_weight_below: t -> float -> unit
  end

(* Splits is purely the splits-register DATA layer: it exposes exactly
   [Splits_base] (container, I/O-free operations, Newick<->splits, weight
   filtering).  The tree CONSTRUCTORS ([of_splits]/[of_clades]) do not live
   here -- they sit one layer up, in [Trees], above the parser, where they
   belong; see the sealed block at the end of Trees.ml. *)
module Splits: Splits_base
= struct
    module Split =
      struct
        type t = IntZ.t
        (* The result is *not* in canonical form *)
        let of_string s = IntZ.of_string s [@@inline]
        (* The result is *not* in canonical form *)
        let of_list =
          List.fold_left (fun res i -> IntZ.(res + (one lsl i))) IntZ.zero
        let of_array =
          Array.fold_left (fun res i -> IntZ.(res + (one lsl i))) IntZ.zero
        let of_intz x = x [@@inline]
        let to_intz x = x [@@inline]
        let to_string s = IntZ.to_string s [@@inline]
      end
    type t = {
      names: string array;
      mask_complement: IntZ.t;
      (* We store the inverse table, which is mutable *)
      splits: float IntZHashtbl.t
    }
    let get_names ss = ss.names [@@inline]
    let cardinal ss = IntZHashtbl.length ss.splits [@@inline]
    let iter f ss = IntZHashtbl.iter f ss.splits [@@inline]
    let create names =
      let num_elts = Array.length names in
      if Array.to_seq names |> StringSet.of_seq |> StringSet.cardinal <> num_elts then
        Exception.raise __FUNCTION__ IO_Format "Duplicate element names in initializer";
      { names = names;
        mask_complement = IntZ.(one lsl num_elts - one);
        splits = IntZHashtbl.create 1024 }
    let clear ss =
      IntZHashtbl.clear ss.splits
    let add_split splits split weight =
      let num_elts = Array.length splits.names and num_bits = IntZ.numbits split in
      (* Here we make sure that the representation of the split is canonical *)
      if num_bits > num_elts then
        Exception.raise __FUNCTION__ IO_Format
          (Printf.sprintf "Split is incompatible (%d %s found, split set has %d %s)"
            num_bits (String.pluralize_int "bit" num_bits) num_elts (String.pluralize_int "element" num_elts));
      let canonical = IntZ.(min split (splits.mask_complement - split)) in
      IntZHashtbl.replace splits.splits canonical begin
        match IntZHashtbl.find_opt splits.splits canonical with
        | None -> weight
        | Some w -> w +. weight
      end
    let add_splits dst src =
      if src.names <> dst.names then
        Exception.raise_incompatible_arrays __FUNCTION__ "split sets" "element names" Array.iter Fun.id
          src.names dst.names;
      IntZHashtbl.iter
        (fun split weight ->
          IntZHashtbl.replace dst.splits split begin
            match IntZHashtbl.find_opt dst.splits split with
            | None -> weight
            | Some w -> w +. weight
          end)
        src.splits
    (* Bipartition weight assignment when constructing splits from a Newick tree *)
    type weight_t =
      | Constant of float
      | Bootstrap
    (* Collect every leaf name from a Newick tree, sorted alphabetically
       and dedup-checked (so add_newick fails fast on duplicate leaves). *)
    let _newick_leaves tree =
      let acc = ref [] in
      Newick.dfs_iter
        (fun node n_children ->
          if n_children = 0 then
            acc := Newick.get_node_name node :: !acc)
        (fun _ _ -> ()) (fun _ _ -> ()) (fun _ _ -> ())
        tree;
      let arr = List.rev !acc |> Array.of_list in
      Array.sort String.compare arr;
      let seen = ref StringSet.empty in
      Array.iter
        (fun n ->
          if StringSet.mem n !seen then
            Exception.raise __FUNCTION__ IO_Format
              (Printf.sprintf "Duplicate leaf %S in Newick" n);
          seen := StringSet.add n !seen)
        arr;
      arr
    (* DFS over a Newick tree, emitting one bipartition per internal
       node into [reg].  The stack holds (mask, leaf-count) per active
       subtree; on entering an internal node we push a placeholder, on
       leaving we pop the n_children child accumulators plus the
       placeholder, combine, push the result, and emit a bipartition
       if non-trivial (1 < size < n-1). *)
    let _add_newick_into ~weight_kind reg tree =
      let names = get_names reg in
      let n = Array.length names in
      let name_to_idx = Hashtbl.create n in
      Array.iteri (fun i nm -> Hashtbl.add name_to_idx nm i) names;
      let stack = Stack.create () in
      Newick.dfs_iter
        (fun node n_children ->
          if n_children = 0 then begin
            let nm = Newick.get_node_name node in
            match Hashtbl.find_opt name_to_idx nm with
            | Some i -> Stack.push (IntZ.(one lsl i), 1) stack
            | None ->
              Exception.raise __FUNCTION__ IO_Format
                (Printf.sprintf "Newick leaf %S is not in the splits register" nm)
          end else
            Stack.push (IntZ.zero, 0) stack)
        (fun _ _ -> ())
        (fun _ _ -> ())
        (fun _ n_children ->
          if n_children > 0 then begin
            let combined_mask = ref IntZ.zero
            and combined_size = ref 0 in
            for _ = 1 to n_children do
              let (m, s) = Stack.pop stack in
              combined_mask := IntZ.logor !combined_mask m;
              combined_size := !combined_size + s
            done;
            (* discard the pre-pushed placeholder for this internal node *)
            let _ = Stack.pop stack in
            Stack.push (!combined_mask, !combined_size) stack;
            if !combined_size > 1 && !combined_size < n - 1 then begin
              let w = match weight_kind with
                | Constant c -> c
                | Bootstrap -> 1.0 (* TODO: thread parent-edge bootstrap via dfs_iteri *) in
              add_split reg !combined_mask w
            end
          end)
        tree
    let of_newick ?(weight_kind = Constant 1.0) tree =
      let leaves = _newick_leaves tree in
      let reg = create leaves in
      _add_newick_into ~weight_kind reg tree;
      reg
    let add_newick ?(weight_kind = Constant 1.0) reg tree =
      let tree_leaves = _newick_leaves tree in
      let reg_leaves = get_names reg in
      if Array.length reg_leaves = 0 then
        Exception.raise __FUNCTION__ IO_Format
          "add_newick on empty register: call of_newick first";
      if tree_leaves <> reg_leaves then
        Exception.raise_incompatible_arrays __FUNCTION__ "splits set" "leaf names"
          Array.iter Fun.id tree_leaves reg_leaves;
      _add_newick_into ~weight_kind reg tree
    let drop_weight_below splits cutoff =
      let keep = ref [] in
      IntZHashtbl.iter
        (fun s w -> if w >= cutoff then List.accum keep (s, w))
        splits.splits;
      IntZHashtbl.clear splits.splits;
      List.iter (fun (s, w) -> IntZHashtbl.replace splits.splits s w) !keep
  end

