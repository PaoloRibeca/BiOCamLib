(*
    Trees.ml -- (c) 2024-2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Trees.ml implements tools to represent and process phylogenetic trees.

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

(* Complete modules including parser(s) and I/O *)

module Newick:
  sig
    include module type of Trees_Base.Newick
    (* Input.  [negative_branches] tells the reader what to do
       on encountering a negative branch length (e.g. NJ-induced
       noise); see [Trees_Base.Newick.NegativeBranchesPolicy.t].
       Default is [Error] for backwards compatibility. *)
    val of_string: ?rich_format:bool ->
                   ?negative_branches:NegativeBranchesPolicy.t ->
                   string -> t
    val array_of_string: ?rich_format:bool ->
                         ?negative_branches:NegativeBranchesPolicy.t ->
                         string -> t array
    val of_file: ?rich_format:bool ->
                 ?negative_branches:NegativeBranchesPolicy.t ->
                 string -> t
    val array_of_file: ?rich_format:bool ->
                       ?negative_branches:NegativeBranchesPolicy.t ->
                       string -> t array
    (* Output *)
    val to_string: ?rich_format:bool -> t -> string
    val array_to_string: ?rich_format:bool -> t array -> string
    val to_file: ?rich_format:bool -> t -> string -> unit
    val array_to_file: ?rich_format:bool -> t array -> string -> unit
  end
= struct
    include Trees_Base.Newick
    let _of_string ?(rich_format = true)
                   ?(negative_branches = NegativeBranchesPolicy.Error) f s =
      (* This adds an implicit unrooted token to the first tree *)
      let s = "\n" ^ s
      and state = Trees_Lex.Newick.create ~rich_format ~negative_branches () in
      f (Trees_Lex.newick state) (Lexing.from_string ~with_positions:true s)
    let of_string ?(rich_format = true) ?(negative_branches = NegativeBranchesPolicy.Error) s =
      _of_string ~rich_format ~negative_branches Trees_Parse.newick_tree s
    let array_of_string ?(rich_format = true) ?(negative_branches = NegativeBranchesPolicy.Error) s =
      _of_string ~rich_format ~negative_branches
        Trees_Parse.zero_or_more_newick_trees s |> Array.of_list
    let _of_file ?(rich_format = true) ?(negative_branches = NegativeBranchesPolicy.Error) f s =
      (* Here we have to reimplement buffering due to the initial unrooted tag *)
      let buf = ref "\n" and ic = open_in s and eof_reached = ref false in
      let lexbuf payload n =
        let res = ref 0 in
        while !res = 0 && not !eof_reached do
          let len = String.length !buf in
          if len > 0 then begin
            res := min len n;
            (*Printf.eprintf "READ='%s'\n%!" !buf;*)
            String.blit !buf 0 payload 0 !res;
            buf := String.sub !buf !res (len - !res)
          end else begin
            (* Here buf is empty *)
            res := input ic payload 0 n;
            (*Printf.eprintf "READ='%s'\n%!" (String.sub payload 0 !res);*)
            if !res = 0 then begin
              close_in ic;
              eof_reached := true
            end
          end
        done;
        (* Here either res > 0 or !eof_reached == true *)
        (*Printf.eprintf "RES(%d)='%s'\n%!" !res (String.escaped (String.sub payload 0 !res));*)
        !res
      and state = Trees_Lex.Newick.create ~rich_format ~negative_branches () in
      f (Trees_Lex.newick state) (Lexing.from_function ~with_positions:true lexbuf)
    let of_file ?(rich_format = true) ?(negative_branches = NegativeBranchesPolicy.Error) s =
      _of_file ~rich_format ~negative_branches Trees_Parse.newick_tree s
    let array_of_file ?(rich_format = true) ?(negative_branches = NegativeBranchesPolicy.Error) s =
      _of_file ~rich_format ~negative_branches
        Trees_Parse.zero_or_more_newick_trees s |> Array.of_list
    let add_to_buffer ?(rich_format = true) buf t =
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
      begin match rich_format, get_is_root t with
      | true, true -> Buffer.add_string buf "[&R]"
      | true, false -> Buffer.add_string buf "[&U]"
      | false, _ -> ()
      end;
      dfs_iter
        (fun _ num_edges ->
          if num_edges > 0 then
            Buffer.add_char buf '(')
        (fun i _ ->
          if i > 0 then
            Buffer.add_char buf ',')
        (fun _ edge ->
          let dict = get_edge_dict edge in
          let l, b, p = get_edge_values edge in
          let l_def = l <> neg_infinity
          and b_def = b <> -1.
          and p_def = p <> -1. in
          begin match l_def, b_def, p_def with
          | false, false, false ->
            if dict <> StringMap.empty then
              (* The only unambiguous way to print out things
                 here is by adding an empty length. *)
              Buffer.add_string buf ":0"
          | false, false, true  -> Printf.bprintf buf ":::%.10g" p
          | false, true,  false -> Printf.bprintf buf "::%.10g" b
          | true,  false, false -> Printf.bprintf buf ":%.10g" l
          | false, true,  true  -> Printf.bprintf buf "::%.10g:%.10g" b p
          | true,  false, true  -> Printf.bprintf buf ":%.10g::%.10g" l p
          | true,  true,  false -> Printf.bprintf buf ":%.10g:%.10g" l b
          | true,  true,  true  -> Printf.bprintf buf ":%.10g:%.10g:%.10g" l b p
          end;
          add_dict_info buf dict)
        (fun node num_edges ->
          if num_edges > 0 then
            Buffer.add_char buf ')';
          get_node_name node |> Buffer.add_string buf;
          get_node_hybrid node |> add_hybrid_info buf;
          get_node_dict node |> add_dict_info buf)
        t;
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

module Splits:
  sig
    (* Re-export only the data operations: the tree constructors
       [to_tree]/[tree_of_clades] are intentionally NOT surfaced here --
       the public entry points are [Trees.of_splits] / [Trees.of_clades]. *)
    include Trees_Base.Splits_base
      with type t = Trees_Base.Splits.t
       and module Split = Trees_Base.Splits.Split
    (* Input *)
    val of_string: string -> t
    val array_of_string: string -> t array
    val of_file: string -> t
    val array_of_file: string -> t array
    val of_channel: in_channel -> t
    val of_binary: ?verbose:bool -> string -> t
    (* Read a Newick file (single- or multi-tree) and build a Splits
        register from its bipartitions.  Each tree's non-trivial
        bipartitions are accumulated (matching the semantics of
        Splits.add_newick: weights of identical bipartitions sum).
       The negative_branches policy controls handling of negative
        branch lengths in the input (default OK: accept silently). *)
    val of_newick_file: ?negative_branches:Newick.NegativeBranchesPolicy.t ->
                        ?weight_kind:weight_t -> string -> t
    val add_newick_file: ?negative_branches:Newick.NegativeBranchesPolicy.t ->
                         ?weight_kind:weight_t -> t -> string -> unit
    (* Output *)
    val to_string: ?precision:int -> t -> string
    val array_to_string: ?precision:int -> t array -> string
    val to_file: ?precision:int -> t -> string -> unit
    val array_to_file: ?precision:int -> t array -> string -> unit
    val to_channel: out_channel -> t -> unit
    val to_binary: ?verbose:bool -> t -> string -> unit
  end
= struct
    include Trees_Base.Splits
    (* Input *)
    let _of_string f s =
      let state = Trees_Lex.Splits.create () in
      f (Trees_Lex.splits state) (Lexing.from_string ~with_positions:true s)
    let of_string = _of_string Trees_Parse.split_set
    let array_of_string s = _of_string Trees_Parse.zero_or_more_split_sets s |> Array.of_list
    let make_filename_text = function
      | w when String.length w >= 5 && String.sub w 0 5 = "/dev/" -> w
      | prefix -> prefix ^ ".PhyloSplits.txt"
    let _of_file f prefix =
      let path = make_filename_text prefix in
      let input = open_in path and state = Trees_Lex.Splits.create () in
      let res = f (Trees_Lex.splits state) (Lexing.from_channel ~with_positions:true input) in
      close_in input;
      res
    let of_file = _of_file Trees_Parse.split_set
    let array_of_file s = _of_file Trees_Parse.zero_or_more_split_sets s |> Array.of_list
    (* The negative_branches policy is exposed so the caller can choose
       whether to error, accept, or clamp to zero on negative branch
       lengths.  For NJ-style consumption the typical choice is OK. *)
    let of_newick_file ?(negative_branches = Newick.NegativeBranchesPolicy.OK) ?weight_kind path =
      let trees = Newick.array_of_file ~negative_branches path in
      if Array.length trees = 0 then
        Exception.raise __FUNCTION__ IO_Format
          (Printf.sprintf "No Newick trees in %S" path);
      let reg = Trees_Base.Splits.of_newick ?weight_kind trees.(0) in
      for i = 1 to Array.length trees - 1 do
        Trees_Base.Splits.add_newick ?weight_kind reg trees.(i)
      done;
      reg
    let add_newick_file ?(negative_branches = Newick.NegativeBranchesPolicy.OK) ?weight_kind reg path =
      let trees = Newick.array_of_file ~negative_branches path in
      Array.iter (fun t -> Trees_Base.Splits.add_newick ?weight_kind reg t) trees
    (* Output *)
    let add_to_buffer ?(precision = 15) buf t =
      let names = get_names t in
      if Array.length names > 0 then begin
        Array.iteri
          (fun i name ->
            if i > 0 then
              Buffer.add_char buf ' ';
            Trees_Lex.quote_string_if_needed name |> Buffer.add_string buf)
          names;
        let num_splits = cardinal t in
        if num_splits > 0 then begin
          Buffer.add_char buf ':';
          iter (fun split weight -> Printf.bprintf buf " 0d%s#%.*g" (Split.to_string split) precision weight) t
        end;
        Buffer.add_char buf ';'
      end;
      buf
    let to_string ?(precision = 15) t =
      add_to_buffer ~precision (Buffer.create 1024) t |> Buffer.contents
    let array_to_string ?(precision = 15) a =
      let buf = Buffer.create 1024 in
      Array.iter (fun t -> Buffer.add_char (add_to_buffer ~precision buf t) '\n') a;
      Buffer.contents buf
    let to_file ?(precision = 15) t prefix =
      let path = make_filename_text prefix in
      let output = open_out path and buf = Buffer.create 1024 in
      Buffer.add_char (add_to_buffer ~precision buf t) '\n';
      Buffer.output_buffer output buf;
      close_out output
    let array_to_file ?(precision = 15) a prefix =
      let path = make_filename_text prefix in
      let output = open_out path and buf = Buffer.create 1024 in
      Array.iter (fun t -> Buffer.add_char (add_to_buffer ~precision buf t) '\n') a;
      Buffer.output_buffer output buf;
      close_out output
    (* *)
    let archive_version = "2025-02-05"
    (* *)
    let make_filename_binary = function
      | w when String.length w >= 5 && String.sub w 0 5 = "/dev/" -> w
      | prefix -> prefix ^ ".PhyloSplits"
    let to_channel output ss =
      archive_version |> output_value output;
      output_value output ss
    let to_binary ?(verbose = false) ss prefix =
      let path = make_filename_binary prefix in
      let output = open_out path in
      if verbose then
        Printf.eprintf "(%s): Outputting database to file '%s'...%!" __FUNCTION__ path;
      to_channel output ss;
      close_out output;
      if verbose then
        Printf.eprintf " done.\n%!"
    let of_channel input =
      let version = (input_value input: string) in
      if version <> archive_version then
        Exception.raise_incompatible_archive_version __FUNCTION__ version archive_version;
      (input_value input: t)
    let of_binary ?(verbose = false) prefix =
      let path = make_filename_binary prefix in
      let input = open_in path in
      if verbose then
        Printf.eprintf "(%s): Reading database from file '%s'...%!" __FUNCTION__ path;
      let res = of_channel input in
      close_in input;
      if verbose then
        Printf.eprintf " done.\n%!";
      res
  end

(* Tree constructors -- the headline entry points for turning splits into a
    tree.  [of_splits] takes an arbitrary weighted split register, runs the
    compatibility filter, and returns (used_splits, tree, unused_splits).
    [of_clades] builds a tree directly from a sparse, laminar-BY-CONSTRUCTION
    family of clades (each = the leaf indices of its smaller side + a weight);
    it does no weighted selection and RAISES on a non-laminar family.
   They live HERE, not in [Splits]: a tree constructor is not a splits-register
    data operation (indeed [of_clades] never builds a register at all), and it
    sits conceptually above the splits parser.  They reach the split bitmasks
    only through the public [Splits] API ([iter]/[cardinal]/[create]/[add_split]
    plus the [Split.to_intz]/[Split.of_intz] bridge), so the register's
    representation stays encapsulated.  The shared union-find core and its
    helpers are sealed away by the signature below -- only [of_splits] and
    [of_clades] are exported. *)
include (struct
    module SplitsRMultimap = Tools.Multimap (RComparableFloat) (ComparableIntZ)
    (* Iterate the indices of the SET bits of [mask], lowest first, in time
       proportional to the popcount (not to the bit-width): we repeatedly read
       the lowest set bit and clear it with [m land (m - 1)]. *)
    let iter_set_bits f mask =
      let m = ref mask in
      while not (IntZ.equal !m IntZ.zero) do
        f (IntZ.trailing_zeros !m);
        m := IntZ.(logand !m (!m - one))
      done
    (* Reconstruct a tree from a family of clades -- the smaller sides of a
        set of splits.  [names] are the [n] leaves (index [i] <-> [names.(i)]);
        [clades] is a list of [(tag, cardinality, weight, members)], where
        [members f] applies [f] to every leaf index of that clade and [tag] is
        an opaque caller label, used only to report which clades were dropped.
       Clades are bucket-sorted by cardinality (smallest first) and merged
        with a union-find: when we reach a clade, every clade strictly inside
        it has already been collapsed to one group, so the distinct current
        roots among its members are exactly the children of its node.  Each
        clade's weight goes on the edge ABOVE its node (the edge the split
        creates), carried as a per-group "stem" weight until the group is
        adopted by a larger clade or joined at the centre.
       Compatibility is checked on the fly, for free: the union-find sets are
        disjoint, so the sizes of the distinct roots found inside a clade sum
        to AT LEAST the clade's cardinality, with equality iff every such root
        lies entirely within the clade.  A strict excess means some root
        STRADDLES the clade boundary -- the clade is incompatible with what has
        already been accepted -- and the handling depends on [strict]:
         - [strict = false] (default): drop the clade, collect its [tag], and
           return the dropped tags alongside the tree.  [of_splits] uses this --
           its colour pre-filter can over-accept, and the dropped tags are
           reconciled back into its unused-splits partition.
         - [strict = true]: raise at once.  [of_clades] uses this -- its caller
           promises a laminar family (e.g.\ a clustering dendrogram), so an
           incompatible clade is a caller error, not something to paper over.
       Returns [(tree, dropped_tags)] ([dropped_tags] is empty when [strict]).
       Cost is O(N alpha(n) + n + m), with N the total clade cardinality. *)
    let assemble_clades ?(verbose = false) ?(strict = false) names n clades =
      if n = 0 then Newick.leaf "", [] else begin
        let uf = Array.init n (fun i -> i) and sz = Array.make n 1 in
        let rec find i =
          if uf.(i) = i then i else (let r = find uf.(i) in uf.(i) <- r; r) in
        let subtree = Array.init n (fun i -> Newick.leaf names.(i))
        and stem = Array.make n None in
        (* Counting sort of the clades by cardinality (keeping the true card) *)
        let buckets = Array.make (n + 1) [] in
        List.iter
          (fun (tag, card, w, members) ->
            let b = if card < 1 then 1 else if card > n then n else card in
            buckets.(b) <- (tag, card, w, members) :: buckets.(b))
          clades;
        (* Per-clade dedup of roots via a monotone stamp, so no array resets *)
        let mark = Array.make n (-1) and stamp = ref 0 and dropped = ref [] in
        for k = 1 to n do
          List.iter
            (fun (tag, card, w, members) ->
              incr stamp;
              let reps = ref [] and total = ref 0 in
              members
                (fun i ->
                  let r = find i in
                  if mark.(r) <> !stamp then begin
                    mark.(r) <- !stamp;
                    reps := r :: !reps;
                    total := !total + sz.(r)
                  end);
              if !total <> card then begin
                (* A root straddles the clade boundary: incompatible *)
                if strict then
                  Exception.raise __FUNCTION__ Algorithm
                    "Clade family is not laminar (an incompatible clade was found)"
                else
                  List.accum dropped tag
              end
              else match !reps with
                | [] | [_] ->
                  (* Clade already realised (duplicate): nothing to do *)
                  ()
                | r0 :: rest ->
                  let node =
                    (r0 :: rest)
                    |> List.rev_map (fun r -> Newick.edge ?length:stem.(r) (), subtree.(r))
                    |> Array.of_list |> Newick.join in
                  List.iter
                    (fun r ->
                      let a = find r0 and b = find r in
                      if a <> b then begin
                        let lo, hi = if sz.(a) < sz.(b) then a, b else b, a in
                        uf.(lo) <- hi;
                        sz.(hi) <- sz.(hi) + sz.(lo)
                      end)
                    rest;
                  let root = find r0 in
                  subtree.(root) <- node;
                  stem.(root) <- Some w)
            buckets.(k)
        done;
        let nd = List.length !dropped in
        if verbose && nd > 0 then
          Printf.eprintf
            "(%s): dropped %d incompatible clade%s during reconstruction\n%!"
            __FUNCTION__ nd (if nd = 1 then "" else "s");
        (* Leftover components -- including the never-merged reference leaf --
           are the branches at the unrooted centre *)
        incr stamp;
        let roots = ref [] in
        for i = 0 to n - 1 do
          let r = find i in
          if mark.(r) <> !stamp then begin
            mark.(r) <- !stamp;
            roots := r :: !roots
          end
        done;
        let tree =
          match !roots with
          | [ r ] -> subtree.(r)
          | rs ->
            rs
            |> List.rev_map (fun r -> Newick.edge ?length:stem.(r) (), subtree.(r))
            |> Array.of_list |> Newick.join in
        tree, !dropped
      end
    (* Compatibility check (Buneman): a candidate split is compatible with the
        set of currently accepted splits iff AT MOST ONE existing colour class
        has elements on both sides of the candidate.  Each colour class is the
        equivalence class of elements under the accepted splits; a class
        straddling the new split would force the existing tree to branch in
        two independent places, which violates pairwise/joint compatibility.
       A single straddling class is fine -- it just refines that branch by
        one extra bit.  The previous implementation checked the weaker
        "either side sees >= 2 colours" condition, which rejected genuine
        refinements (e.g. the nested chain of splits emitted by the hdbscan
        algorithm) and so under-built the tree.  Cost is O(n) per candidate,
        same as before.
       The split bitmasks are reached through the public [Splits] API: we
        iterate with [Splits.iter] and project each split to its IntZ via
        [Split.to_intz], and re-wrap accepted/rejected masks with [Split.of_intz]
        when building the result registers.  [mask_complement] is recomputed
        from the element count rather than read off the register. *)
    let of_splits ?(verbose = false) splits =
      let names = Splits.get_names splits in
      let num_elts = Array.length names in
      let mask_complement = IntZ.(one lsl num_elts - one) in
      (* Sort splits in iteration order.  The colour-reconstruction loop
          below right-shifts colours one bit at a time, undoing splits in
          REVERSE-of-acceptance order: the LATEST-accepted split is
          undone FIRST, merging the two leaf groups that were on its two
          sides.  For the resulting tree to be correct, the LATEST-
          accepted split must be the DEEPEST in the tree (e.g.\ a cherry
          like {A, B}), and the FIRST-accepted split must be the most
          CENTRAL (largest smaller-side cardinality).
         The original implementation sorted only by weight; with equal
          weights (e.g.\ all 1.0 from Newick-derived ensemble unions),
          the iteration order was determined by the priority queue's
          IntZ tiebreaking, which has no relation to tree depth.  The
          result was a tree whose bipartitions did not match the input
          splits even though every split was accepted as compatible
          (Buneman's theorem guarantees compatibility, not order).
         Fix: secondary sort key is the smaller-side cardinality
          DESCENDING -- deepest (most central) splits first, cherries
          last.  Tertiary key on the mask itself is purely for
          deterministic output. *)
      let sorted_arr = Array.make (Splits.cardinal splits) (IntZ.zero, 0., 0) in
      let i = ref 0 in
      Splits.iter
        (fun split weight ->
          let split = Splits.Split.to_intz split in
          let pop = IntZ.popcount split in
          let smaller_side = min pop (num_elts - pop) in
          sorted_arr.(!i) <- (split, weight, smaller_side);
          incr i)
        splits;
      Array.sort
        (fun (s1, w1, sz1) (s2, w2, sz2) ->
          let c = Stdlib.compare w2 w1 in
          if c <> 0 then c
          else let c = Stdlib.compare sz2 sz1 in
               if c <> 0 then c
               else IntZ.compare s1 s2)
        sorted_arr;
      let red_num_elts = num_elts - 1 and colors = Array.make num_elts IntZ.zero and num_colors = ref 1
      (* We partition splits based on their compatibility *)
      and ok = ref SplitsRMultimap.empty and ok_weights = ref []
      and ko = ref SplitsRMultimap.empty in
      Array.iter
        (fun (split, weight, _) ->
          let add_split_to partition =
            partition := SplitsRMultimap.add weight split !partition in
          (* Do we need more splits? If colours are all different, we have found enough *)
          if !num_colors >= num_elts then
            add_split_to ko
          else
            (* Is the split compatible with current colours?  For each colour
               class, record whether we have seen it on side 0, side 1, or
               both.  We abort as soon as TWO classes are observed to
               straddle the new split. *)
            try
              let straddle = IntZHashtbl.create 16 in
              let straddling_count = ref 0 in
              for i = 0 to red_num_elts do
                let bit = IntZ.testbit split i in
                let c = colors.(i) in
                let s1, s0 =
                  try IntZHashtbl.find straddle c
                  with Not_found -> false, false in
                let s1' = s1 || bit and s0' = s0 || not bit in
                if not (s1 && s0) && s1' && s0' then begin
                  incr straddling_count;
                  if !straddling_count > 1 then
                    raise_notrace Exit
                end;
                IntZHashtbl.replace straddle c (s1', s0')
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
        sorted_arr;
      if verbose then
        Printf.eprintf "(%s): Found %d colors for %d elements, used %d/%d splits\n%!"
          __FUNCTION__ !num_colors num_elts
          (SplitsRMultimap.cardinal !ok) (SplitsRMultimap.cardinal !ko);
      (* To return used and unused splits, we need to invert tables *)
      let partition_to_splits partition =
        let res = Splits.create names in
        SplitsRMultimap.iter
          (fun weight split -> Splits.add_split res (Splits.Split.of_intz split) weight)
          !partition;
        res in
      (* Reconstruction.  The colour array is used only for the compatibility
         check above; we do NOT reuse it to rebuild the tree (the colour-shift
         recovery interprets bit position as tree depth, which is only valid
         for a rooted, fully-resolved, depth-ordered split chain and otherwise
         over-merges polytomies into spurious sub-clades).  Instead we feed the
         accepted splits to [assemble_clades]: for each one we take its SMALLER
         side -- the canonical clade -- which for a compatible set is laminar,
         and a single smallest-first union-find pass over the set bits recovers
         exactly the input bipartitions, weights on the correct edges, in time
         linear in the total clade cardinality. *)
      let _ = colors and _ = ok_weights in
      let clades = ref [] in
      SplitsRMultimap.iter
        (fun w s ->
          let pop = IntZ.popcount s in
          let smaller, k =
            if pop <= num_elts - pop then s, pop
            else IntZ.(mask_complement - s), num_elts - pop in
          (* Tag the clade with its source split [s] so that, should it be
             dropped during reconstruction, it can be moved back from [ok] to
             [ko] below *)
          clades := (s, k, w, (fun f -> iter_set_bits f smaller)) :: !clades)
        !ok;
      let tree, dropped = assemble_clades ~verbose names num_elts !clades in
      (* The colour pre-filter guarantees pairwise (Buneman) compatibility, but
         [assemble_clades]'s smallest-first union-find can still meet a split
         that straddles an already-built clade when the accepted set is not
         globally laminar.  Such splits were dropped from the tree, so move them
         from the kept ([ok]) to the unused ([ko]) partition, keeping the
         returned bookkeeping honest: the tree and the [ok] database agree. *)
      let dropped_set =
        List.fold_left (fun acc s -> IntZSet.add s acc) IntZSet.empty dropped in
      let ok_final = ref SplitsRMultimap.empty and ko_final = ref !ko in
      SplitsRMultimap.iter
        (fun w s ->
          if IntZSet.mem s dropped_set then
            ko_final := SplitsRMultimap.add w s !ko_final
          else
            ok_final := SplitsRMultimap.add w s !ok_final)
        !ok;
      partition_to_splits ok_final, tree, partition_to_splits ko_final
    let of_clades ?(verbose = false) names clades =
      let tagged =
        List.map
          (fun (idx, w) -> ((), Array.length idx, w, (fun f -> Array.iter f idx)))
          clades in
      (* Strict: the caller promises a laminar family (e.g.\ a clustering
         dendrogram), so an incompatible clade is a caller error, not something
         to silently drop *)
      let tree, _ = assemble_clades ~verbose ~strict:true names (Array.length names) tagged in
      tree
  end : sig
    val of_splits: ?verbose:bool -> Splits.t -> Splits.t * Newick.t * Splits.t
    val of_clades: ?verbose:bool -> string array -> (int array * float) list -> Newick.t
  end)

