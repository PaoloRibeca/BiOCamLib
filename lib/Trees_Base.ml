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

module Splits:
  sig
    module Split:
      sig
        type t
        val of_string: string -> t
        val of_list: int list -> t
        val of_array: int array -> t
        val to_string: t -> string
      end
    type t
    val get_names: t -> string array
    val cardinal: t -> int
    val iter: (Split.t -> float -> unit) -> t -> unit
    (* The argument are element names *)
    exception DuplicateNames
    val create: string array -> t
    (* Discards splits while keeping names *)
    val clear: t -> unit
    exception IncompatibleSplit of string * int * int
    val add_split: t -> Split.t -> float -> unit
    exception IncompatibleSplits
    val add_splits: t -> t -> unit
    (* Converts to a tree the largest subset of compatible splits,
        obtained by considering the splits in order of decreasing weight.
       Both the used and unused splits are returned *)
    val to_tree: ?verbose:bool -> t -> t * Newick.t * t
  end
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
    exception DuplicateNames
    let create names =
      let num_elts = Array.length names in
      if Array.to_seq names |> StringSet.of_seq |> StringSet.cardinal <> num_elts then
        raise DuplicateNames;
      { names = names;
        mask_complement = IntZ.(one lsl num_elts - one);
        splits = IntZHashtbl.create 1024 }
    let clear ss =
      IntZHashtbl.clear ss.splits
    exception IncompatibleSplit of string * int * int
    let add_split splits split weight =
      let num_elts = Array.length splits.names and num_bits = IntZ.numbits split in
      (* Here we make sure that the representation of the split is canonical *)
      if num_bits > num_elts then
        IncompatibleSplit (IntZ.to_string split, num_bits, num_elts) |> raise;
      let canonical = IntZ.(min split (splits.mask_complement - split)) in
      IntZHashtbl.replace splits.splits canonical begin
        match IntZHashtbl.find_opt splits.splits canonical with
        | None -> weight
        | Some w -> w +. weight
      end
    exception IncompatibleSplits
    let add_splits dst src =
      if src.names <> dst.names then
        raise IncompatibleSplits;
      IntZHashtbl.iter
        (fun split weight ->
          IntZHashtbl.replace dst.splits split begin
            match IntZHashtbl.find_opt dst.splits split with
            | None -> weight
            | Some w -> w +. weight
          end)
        src.splits
    module SplitsRMultimap = Tools.Multimap (RComparableFloat) (ComparableIntZ)
    type duplication_state_t =
      | ZeroColors
      | OneColor of IntZ.t
      | TwoOrMoreColors
    module ColorsToTrees = Tools.Multimap (ComparableIntZ) (MakeComparable(Newick))
    let to_tree ?(verbose = false) splits =
      (* We invert the table *)
      let num_elts = Array.length splits.names and sorted_splits = ref SplitsRMultimap.empty in
      IntZHashtbl.iter
        (fun split weight ->
          sorted_splits := SplitsRMultimap.add weight split !sorted_splits)
        splits.splits;
      let red_num_elts = num_elts - 1 and colors = Array.make num_elts IntZ.zero and num_colors = ref 1
      (* We partition splits based on their compatibility *)
      and ok = ref SplitsRMultimap.empty and ok_weights = ref []
      and ko = ref SplitsRMultimap.empty in
      SplitsRMultimap.iter
        (fun weight split ->
          let add_split_to partition =
            partition := SplitsRMultimap.add weight split !partition in
          (* Do we need more splits? If colours are all different, we have found enough *)
          if !num_colors >= num_elts then
            add_split_to ko
          else
            (* Is the split compatible with current colours? *)
            try
              let state_zero = ref ZeroColors and state_one = ref ZeroColors in
              for i = 0 to red_num_elts do
                let bit = IntZ.testbit split i in
                if bit then begin
                  (* One *)
                  match !state_one with
                  | ZeroColors ->
                    (* First colour *)
                    state_one := OneColor colors.(i)
                  | OneColor c ->
                    if colors.(i) <> c then begin
                      if !state_zero = TwoOrMoreColors then
                        (* Incompatible split *)
                        raise_notrace Exit
                      else
                        state_one := TwoOrMoreColors
                    end
                  | TwoOrMoreColors -> ()
                end else begin
                  (* Zero *)
                  match !state_zero with
                  | ZeroColors ->
                    (* First colour *)
                    state_zero := OneColor colors.(i)
                  | OneColor c ->
                    if colors.(i) <> c then begin
                      if !state_one = TwoOrMoreColors then
                        (* Incompatible split *)
                        raise_notrace Exit
                      else
                        state_zero := TwoOrMoreColors
                    end
                  | TwoOrMoreColors -> ()
                end
              done;
              (* Compatible split - we update colours and their number *)
              for i = 0 to red_num_elts do
                colors.(i) <- IntZ.(colors.(i) lsl 1 + if testbit split i then one else zero)
              done;
              num_colors := Array.to_seq colors |> IntZSet.of_seq |> IntZSet.cardinal;
              add_split_to ok;
              List.accum ok_weights weight
            with Exit ->
              (* Incompatible split *)
              add_split_to ko)
        !sorted_splits;
      if verbose then
        Printf.eprintf "(%s): Found %d colors for %d elements, used %d/%d splits\n%!"
          __FUNCTION__ !num_colors (Array.length splits.names)
          (SplitsRMultimap.cardinal !ok) (SplitsRMultimap.cardinal !ko);
      (* To return used and unused splits, we need to invert tables *)
      let partition_to_splits partition =
        let res = create splits.names in
        SplitsRMultimap.iter (fun weight split -> add_split res split weight) !partition;
        res in
      (* We build and output the tree corresponding to names, colours, and weights.
         Note that at this point the elements might or might not have distinct colours *)
      let res =
        (* We initialise the result with as many leaves as the elements.
           First we collect leaves by colour... *)
        let colors_to_trees = ref ColorsToTrees.empty in
        Array.iter2
          (fun color name ->
            colors_to_trees := ColorsToTrees.add color (Newick.leaf name) !colors_to_trees)
          colors splits.names;
        (* ...and then we merge all the leaves associated with the same colour, if there is more than one.
           Branches all have length 0 *)
        ColorsToTrees.map_set
          (fun leaves ->
            if ColorsToTrees.ValSet.cardinal leaves > 1 then
              ColorsToTrees.ValSet.elements_array leaves |> Array.map (fun leaf -> Newick.edge (), leaf) |> Newick.join
            else
              ColorsToTrees.ValSet.min_elt leaves)
          !colors_to_trees
        |> ref in
      while IntZMap.cardinal !res > 1 do
        let colors_to_trees = ref ColorsToTrees.empty in
        IntZMap.iter
          (fun color tree ->
            (* We update the colour *)
            colors_to_trees := ColorsToTrees.add IntZ.(color asr 1) tree !colors_to_trees)
          !res;
        (* We merge all the trees associated with the same colour *)
        let branch_length = List.pop ok_weights in
        res := begin
          ColorsToTrees.map_set
            (fun trees ->
              ColorsToTrees.ValSet.elements_array trees
              |> Array.map (fun tree -> Newick.edge ~length:branch_length (), tree) |> Newick.join)
            !colors_to_trees
        end
      done;
      assert (!ok_weights = []);
      partition_to_splits ok, IntZMap.min_binding !res |> snd, partition_to_splits ko
  end

