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

(* A node name as Newick has to spell it: quoted exactly when leaving it bare
   would not read back, with an embedded quote doubled.  Exported because it is
   part of what writing Newick means -- a caller assembling a tree by hand needs
   it for the same reason both writers here do -- and because it is worth being
   able to check directly: it once flagged a space as requiring quotes and then
   dropped the character, so the name came back short and the round trip still
   passed. *)
let quote_name = Trees_Lex.quote_string_if_needed

module Exception =
  struct
    include Exception
    (* The parsers signal a syntax error by raising an exception that carries no
       information, and the lexer signals a token interrupted by the end of the input
       with a message of its own: what the user needs -- where in the input the parse
       stopped, and on what -- is read off the lexing buffer.
       [shift] discounts the characters the reader prepends to the input, so that the
       position is the one in the data the user provided; [path] is empty when the
       input is a string rather than a file *)
    let catch_malformed ?(shift = 0) ?(path = "") __FUNCTION__ kind lexbuf f =
      let raise_malformed () =
        let source =
          if path = "" then
            "input"
          else
            Printf.sprintf "file '%s'" path
        and found =
          match Lexing.lexeme lexbuf with
          | "" -> "unexpected end of input"
          | token -> Printf.sprintf "unexpected '%s'" token in
        Exception.raise __FUNCTION__ IO_Format
          (Printf.sprintf "At character %d: Malformed %s %s (%s)"
            (Lexing.lexeme_start lexbuf - shift + 1) kind source found) in
      try
        f ()
      with
      | Trees_Parse.Error ->
        raise_malformed ()
      | Failure message when message = "lexing: empty token" ->
        raise_malformed ()
  end

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
      let lexbuf = Lexing.from_string ~with_positions:true s in
      Exception.catch_malformed ~shift:1 __FUNCTION__ "Newick" lexbuf
        (fun () -> f (Trees_Lex.newick state) lexbuf)
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
      let lexbuf = Lexing.from_function ~with_positions:true lexbuf in
      Exception.catch_malformed ~shift:1 ~path:s __FUNCTION__ "Newick" lexbuf
        (fun () -> f (Trees_Lex.newick state) lexbuf)
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
          (* Quoted when it has to be, or a name carrying a space or a colon
             renders as something this library's own reader then rejects.  The
             Splits writer has always done this; the Newick one did not. *)
          get_node_name node |> Trees_Lex.quote_string_if_needed |> Buffer.add_string buf;
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
      let lexbuf = Lexing.from_string ~with_positions:true s in
      Exception.catch_malformed __FUNCTION__ "splits" lexbuf
        (fun () -> f (Trees_Lex.splits state) lexbuf)
    let of_string = _of_string Trees_Parse.split_set
    let array_of_string s = _of_string Trees_Parse.zero_or_more_split_sets s |> Array.of_list
    let make_filename_text = function
      | w when String.length w >= 5 && String.sub w 0 5 = "/dev/" -> w
      | prefix -> prefix ^ ".PhyloSplits.txt"
    let _of_file f prefix =
      let path = make_filename_text prefix in
      let input = open_in path and state = Trees_Lex.Splits.create () in
      let lexbuf = Lexing.from_channel ~with_positions:true input in
      let res =
        Exception.catch_malformed ~path __FUNCTION__ "splits" lexbuf
          (fun () -> f (Trees_Lex.splits state) lexbuf) in
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
      let reg = of_newick ?weight_kind trees.(0) in
      for i = 1 to Array.length trees - 1 do
        add_newick ?weight_kind reg trees.(i)
      done;
      reg
    let add_newick_file ?(negative_branches = Newick.NegativeBranchesPolicy.OK) ?weight_kind reg path =
      let trees = Newick.array_of_file ~negative_branches path in
      Array.iter (fun t -> add_newick ?weight_kind reg t) trees
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
    tree.  [of_splits] takes an arbitrary weighted split register, keeps the
    splits a greedy consensus keeps, and returns (used_splits, tree,
    unused_splits).  [of_clades] builds a tree directly from a sparse,
    laminar-BY-CONSTRUCTION family of clades (each = the leaf indices of its
    smaller side + a weight); it does no weighted selection and RAISES on a
    non-laminar family.
   They live HERE, not in [Splits]: a tree constructor is not a splits-register
    data operation (indeed [of_clades] never builds a register at all), and it
    sits conceptually above the splits parser.  They reach the split bitmasks
    only through the public [Splits] API ([iter]/[cardinal]/[create]/[add_split]
    plus the [Split.to_intz]/[Split.of_intz] bridge), so the register's
    representation stays encapsulated.  The shared union-find core and its
    helpers are sealed away by the signature below -- only [of_splits] and
    [of_clades] are exported. *)
include (
  struct
    (* Iterate the indices of the SET bits of [mask], lowest first.  The mask is read as the
       bytes of its magnitude, so the cost is one pass over its width in bytes, where an empty
       byte costs a comparison, and one step per set bit: clearing the lowest bit of the mask
       instead costs a whole-width operation per bit, about a hundred times more on a split of a
       few thousand elements with half of them set *)
    let lowest_bit_of_byte =
      Array.init 256
        (fun b ->
          let rec lowest i = if i = 8 || b land (1 lsl i) <> 0 then i else lowest (i + 1) in
          lowest 0)
    let iter_set_bits f mask =
      let bytes = IntZ.to_bits mask in
      for i = 0 to String.length bytes - 1 do
        let b = ref (Char.code bytes.[i]) in
        if !b <> 0 then begin
          let base = i lsl 3 in
          while !b <> 0 do
            f (base + lowest_bit_of_byte.(!b));
            b := !b land (!b - 1)
          done
        end
      done
    (* Reconstruct a tree from a laminar family of clades -- the smaller sides
        of a set of compatible splits.  [names] are the [n] leaves (index [i] <->
        [names.(i)]); [clades] is a list of [(cardinality, weight, members)],
        where [members f] applies [f] to every leaf index of that clade.
       Clades are bucket-sorted by cardinality (smallest first) and merged
        with a union-find: when we reach a clade, every clade strictly inside
        it has already been collapsed to one group, so the distinct current
        roots among its members are exactly the children of its node.  Each
        clade's weight goes on the edge ABOVE its node (the edge the split
        creates), carried as a per-group "stem" weight until the group is
        adopted by a larger clade or joined at the centre.
       Laminarity is checked on the fly, for free: the union-find sets are
        disjoint, so the sizes of the distinct roots found inside a clade sum
        to AT LEAST the clade's cardinality, with equality iff every such root
        lies entirely within the clade.  A strict excess means some root
        STRADDLES the clade boundary, so the family was not laminar, and that
        raises: [of_clades]' caller promises a laminar family, and [of_splits]
        hands over only splits it has checked to be compatible.
       Cost is O(N alpha(n) + n + m), with N the total clade cardinality. *)
    let assemble_clades names n clades =
      if n = 0 then Newick.leaf "" else begin
        let uf = Array.init n (fun i -> i) and sz = Array.make n 1 in
        let rec find i =
          if uf.(i) = i then i else (let r = find uf.(i) in uf.(i) <- r; r) in
        let subtree = Array.init n (fun i -> Newick.leaf names.(i))
        and stem = Array.make n None in
        (* Counting sort of the clades by cardinality (keeping the true card) *)
        let buckets = Array.make (n + 1) [] in
        List.iter
          (fun (card, w, members) ->
            let b = if card < 1 then 1 else if card > n then n else card in
            buckets.(b) <- (card, w, members) :: buckets.(b))
          clades;
        (* Per-clade dedup of roots via a monotone stamp, so no array resets *)
        let mark = Array.make n (-1) and stamp = ref 0 in
        for k = 1 to n do
          List.iter
            (fun (card, w, members) ->
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
              if !total <> card then
                (* A root straddles the clade boundary: incompatible *)
                Exception.raise __FUNCTION__ IO_Format
                  "Clade family is not laminar (an incompatible clade was found)"
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
        match !roots with
        | [ r ] -> subtree.(r)
        | rs ->
          rs
          |> List.rev_map (fun r -> Newick.edge ?length:stem.(r) (), subtree.(r))
          |> Array.of_list |> Newick.join
      end
    (* A consensus of splits, greedily: the splits are taken in decreasing
        weight, and each is kept iff it is compatible with every split kept
        before it -- two bipartitions A|A' and B|B' being compatible when one of
        their four intersections is empty.  Ties in weight go to the larger
        smaller side, the most central split first, and then to the smaller
        mask, which is there only to make the result deterministic.
       Each split is tested exactly, and against the tree rather than against
        the kept splits one by one.  A split is held as its side without element
        0, a cluster of the tree rooted at element 0, and splits are compatible
        iff their clusters are laminar -- any two nested or disjoint -- so the
        kept splits are at every moment a rooted tree, built as they come: the
        elements are its leaves and each kept split one of its nodes.  A cluster
        C fits that tree iff each child of the lowest node u holding all of C
        lies wholly inside C or wholly outside it, since a node below such a
        child is inside or outside with it and a node above u holds all of C;
        keeping C is then hanging the children of u that lie inside it from a
        new node.  Only the nodes on the paths from the elements of C up to the
        root are visited, each of them once: the subtree C induces, which is
        usually a few times |C| and never more than the whole tree.  So a split
        costs one pass over its mask in bytes, O(n / 8), plus the size of that
        subtree, and at most O(n) whatever it is and however many splits have
        been kept.  Once the tree is fully resolved no split can fit it but a
        trivial one, and the rest are turned away unread.
       The tree returned is built from the kept splits by [assemble_clades],
        which puts the weight of each on its edge.  The split bitmasks are
        reached through the public [Splits] API: [Splits.iter] and
        [Split.to_intz] out of the register, [Split.of_intz] back into the two
        registers returned. *)
    let of_splits ?(verbose = false) splits =
      let names = Splits.get_names splits in
      let num_elts = Array.length names in
      let mask_complement = IntZ.(one lsl num_elts - one) in
      let sorted_arr = Array.make (Splits.cardinal splits) (IntZ.zero, 0., 0) and i = ref 0 in
      Splits.iter
        (fun split weight ->
          let split = Splits.Split.to_intz split in
          let pop = IntZ.popcount split in
          sorted_arr.(!i) <- (split, weight, min pop (num_elts - pop));
          incr i)
        splits;
      Array.sort
        (fun (s1, w1, sz1) (s2, w2, sz2) ->
          let c = Float.compare w2 w1 in
          if c <> 0 then c
          else
            let c = Int.compare sz2 sz1 in
            if c <> 0 then c else IntZ.compare s1 s2)
        sorted_arr;
      (* The tree of the kept splits: the elements 0 .. n - 1, the root n, and after it one node
         per kept split.  Children hang in doubly linked lists, so a child moves in constant time *)
      let root = num_elts and max_nodes = 2 * num_elts + 1 in
      let parent = Array.make max_nodes (-1) and first = Array.make max_nodes (-1)
      and next = Array.make max_nodes (-1) and prev = Array.make max_nodes (-1)
      and size = Array.make max_nodes 1 and degree = Array.make max_nodes 0 in
      let link p c =
        parent.(c) <- p;
        prev.(c) <- -1;
        next.(c) <- first.(p);
        if first.(p) >= 0 then prev.(first.(p)) <- c;
        first.(p) <- c;
        degree.(p) <- degree.(p) + 1
      and unlink c =
        let p = parent.(c) in
        if prev.(c) >= 0 then next.(prev.(c)) <- next.(c) else first.(p) <- next.(c);
        if next.(c) >= 0 then prev.(next.(c)) <- prev.(c);
        degree.(p) <- degree.(p) - 1 in
      for e = 0 to num_elts - 1 do
        link root e
      done;
      size.(root) <- num_elts;
      (* What one candidate needs, reset by stamping rather than by clearing: a node belongs to
         the current candidate iff [stamp] holds its number.  [count] is how many of the
         candidate's elements lie below a node, [ihead] and [inext] chain the visited children
         of a visited node, and [order] and [stack] serve the walks *)
      let stamp = Array.make max_nodes 0 and count = Array.make max_nodes 0
      and ihead = Array.make max_nodes (-1) and inext = Array.make max_nodes (-1)
      and order = Array.make max_nodes 0 and stack = Array.make max_nodes 0
      and members = Array.make (max num_elts 1) 0 and candidate = ref 0 and free = ref (root + 1)
      and resolving = ref 0 in
      (* Hang the cluster held in [members.(0 .. s - 1)] if it fits the tree, and say whether *)
      let fits s =
        incr candidate;
        let c = !candidate and visited = ref 0 in
        (* The nodes on the paths up from the members, each visited once *)
        for j = 0 to s - 1 do
          let v = ref members.(j) in
          while !v >= 0 && stamp.(!v) <> c do
            stamp.(!v) <- c;
            count.(!v) <- 0;
            ihead.(!v) <- -1;
            order.(!visited) <- !v;
            incr visited;
            v := parent.(!v)
          done
        done;
        (* Every visited node but the root has a visited parent, since each walk went on up to
           the root or to a node already visited *)
        for k = 0 to !visited - 1 do
          let v = order.(k) in
          let p = parent.(v) in
          if p >= 0 then begin
            inext.(v) <- ihead.(p);
            ihead.(p) <- v
          end
        done;
        (* The counts, children first: the visited nodes in preorder from the root, read back *)
        let top = ref 1 and seen = ref 0 in
        stack.(0) <- root;
        while !top > 0 do
          decr top;
          let v = stack.(!top) in
          order.(!seen) <- v;
          incr seen;
          let w = ref ihead.(v) in
          while !w >= 0 do
            stack.(!top) <- !w;
            incr top;
            w := inext.(!w)
          done
        done;
        for k = !seen - 1 downto 0 do
          let v = order.(k) in
          if v < num_elts then count.(v) <- 1;
          let p = parent.(v) in
          if p >= 0 then count.(p) <- count.(p) + count.(v)
        done;
        (* The lowest node holding every member: at most one child of a node holds them all *)
        let u = ref root and descending = ref true in
        while !descending do
          let w = ref ihead.(!u) in
          while !w >= 0 && count.(!w) <> s do
            w := inext.(!w)
          done;
          if !w >= 0 then u := !w else descending := false
        done;
        (* It fits iff each child of that node holding a member holds nothing else *)
        let inside = ref 0 and w = ref ihead.(!u) in
        while !w >= 0 && count.(!w) = size.(!w) do
          incr inside;
          w := inext.(!w)
        done;
        let fit = !w < 0 in
        (* With one child inside, or all of them, the cluster is a node already *)
        if fit && !inside > 1 && !inside < degree.(!u) then begin
          let node = !free in
          incr free;
          size.(node) <- s;
          let w = ref ihead.(!u) in
          while !w >= 0 do
            let child = !w in
            w := inext.(child);
            unlink child;
            link node child
          done;
          link !u node;
          incr resolving
        end;
        fit in
      (* The kept splits go into one register and the rest into another; the kept ones are
         compatible, so their smaller sides are a laminar family and make the tree's clades *)
      let kept = Splits.create names and rejected = Splits.create names and clades = ref [] in
      Array.iter
        (fun (split, weight, smaller) ->
          let keep () =
            Splits.add_split kept (Splits.Split.of_intz split) weight;
            let pop = IntZ.popcount split in
            let side = if pop <= num_elts - pop then split else IntZ.(mask_complement - split) in
            List.accum clades (smaller, weight, (fun f -> iter_set_bits f side))
          and reject () = Splits.add_split rejected (Splits.Split.of_intz split) weight
          (* The side without element 0 *)
          and cluster = if IntZ.testbit split 0 then IntZ.(mask_complement - split) else split in
          let s = IntZ.popcount cluster in
          if s <= 1 || s >= num_elts - 1 then
            (* One element against the rest, or none: it fits any tree and changes none *)
            keep ()
          else if !resolving >= num_elts - 3 then
            (* The tree is fully resolved, and nothing more can fit it *)
            reject ()
          else begin
            let j = ref 0 in
            iter_set_bits (fun e -> members.(!j) <- e; incr j) cluster;
            if fits s then keep () else reject ()
          end)
        sorted_arr;
      if verbose then begin
        let num_splits = Array.length sorted_arr in
        Printf.eprintf "(%s): kept %d of %d %s, %d of them resolving the tree\n%!" __FUNCTION__
          (Splits.cardinal kept) num_splits (String.pluralize_int "split" num_splits) !resolving
      end;
      kept, assemble_clades names num_elts !clades, rejected
    (* The caller promises a laminar family (e.g.\ a clustering dendrogram), so
        an incompatible clade is the caller's error, and raises *)
    let of_clades ?(verbose = false) names clades =
      let tree =
        assemble_clades names (Array.length names)
          (List.map (fun (idx, w) -> Array.length idx, w, (fun f -> Array.iter f idx)) clades) in
      if verbose then begin
        let n = List.length clades in
        Printf.eprintf "(%s): assembled %d %s\n%!" __FUNCTION__ n (String.pluralize_int "clade" n)
      end;
      tree
  end: sig
    val of_splits: ?verbose:bool -> Splits.t -> Splits.t * Newick.t * Splits.t
    val of_clades: ?verbose:bool -> string array -> (int array * float) list -> Newick.t
  end
)

(* Neighbour joining (Saitou and Nei 1987) with the Studier and Keppler 1988
   formulation of the criterion, which is what makes each step O(m^2) rather
   than O(m^3): the row sums are kept as we go instead of being recomputed.
   The tree it returns is UNROOTED -- the last three subtrees are resolved in
   closed form into a trifurcation, which is where an unrooted tree's arbitrary
   Newick top node belongs -- so a caller that wants a rooted one should follow
   with [Newick.midpoint_root]. *)
module NeighbourJoining:
  sig
    (* What to do when the matrix disagrees with itself across the diagonal.
       Neighbour joining is defined on a symmetric matrix, and a real one often
       is not quite: [Average] replaces both cells with their mean, [Error]
       refuses the matrix instead.  Canonical CLI forms are 'average'|'error' *)
    module AsymmetryPolicy:
      sig
        type t =
          | Average
          | Error
        val of_string: string -> t
        val to_string: t -> string
      end
    (* What a distance matrix looks like before anything is joined from it.
       Every field is a property of the INPUT, so a matrix that is about to be
       refused can still be described -- which is when a description is most
       useful.  [square] is exactly the condition [of_matrix] imposes, and is
       taken from here rather than tested again, so the two cannot drift apart:
       as many rows as columns, at least one of them, carrying the same names in
       the same order.  The three fields that compare a cell with its mirror
       image mean nothing without it and are [nan] where it does not hold *)
    module Statistics:
      sig
        type t = {
          rows: int;
          columns: int;
          square: bool;
          (* Off-diagonal cells below zero.  A distance is not negative, so a
             matrix with any is either not one or is writing "not measured" *)
          negative_cells: int;
          (* The largest |d(i,i)|, which a distance matrix keeps at zero *)
          diagonal_max: float;
          asymmetry_mean: float;
          asymmetry_max: float;
          (* The pair the largest disagreement was found on *)
          asymmetry_at: string * string;
          minimum: float;
          maximum: float
        }
        val of_matrix: Matrix.t -> t
      end
    (* [negative_branches] says what to do about the negative branch lengths a
       non-additive matrix yields; the sentinel for an undefined length is
       [neg_infinity], so keeping them ([OK], the default) is unambiguous *)
    val of_matrix: ?asymmetry:AsymmetryPolicy.t ->
                   ?negative_branches:Newick.NegativeBranchesPolicy.t -> ?verbose:bool ->
                   Matrix.t -> Newick.t
    (* Branches of negative length, and the total the branches of the tree add
       up to -- the two things about a JOINED tree that say how far from
       additive the matrix that produced it was.  Neither is a property of the
       matrix, so neither is in [Statistics.t] *)
    val count_negative_branches: Newick.t -> int
    val total_branch_length: Newick.t -> float
  end
= struct
    module AsymmetryPolicy =
      struct
        type t =
          | Average
          | Error
        let of_string = function
          | "average" -> Average
          | "error" -> Error
          | s -> Exception.raise_unrecognized_initializer __FUNCTION__ "asymmetry policy" s
        let to_string = function
          | Average -> "average"
          | Error -> "error"
      end
    module Statistics =
      struct
        type t = {
          rows: int;
          columns: int;
          square: bool;
          negative_cells: int;
          diagonal_max: float;
          asymmetry_mean: float;
          asymmetry_max: float;
          asymmetry_at: string * string;
          minimum: float;
          maximum: float
        }
        let of_matrix matrix =
          let names = matrix.Matrix.row_names and cols = matrix.Matrix.col_names in
          let rows = Array.length names and columns = Array.length cols in
          let square =
            rows = columns && rows > 0
            && begin
              let same = ref true in
              Array.iteri (fun i name -> if cols.(i) <> name then same := false) names;
              !same
            end in
          (* The diagonal is only a diagonal when the two axes are the same one;
             otherwise cell (i, i) is an ordinary measurement like any other *)
          let negative_cells = ref 0 and diagonal_max = ref 0.
          and minimum = ref infinity and maximum = ref neg_infinity in
          Array.iteri
            (fun i row ->
              for j = 0 to min columns (Float.Array.length row) - 1 do
                let value = Float.Array.get row j in
                if square && i = j then
                  diagonal_max := max !diagonal_max (Float.abs value)
                else begin
                  if value < 0. then
                    incr negative_cells;
                  if value < !minimum then
                    minimum := value;
                  if value > !maximum then
                    maximum := value
                end
              done)
            matrix.Matrix.data;
          let asymmetry_mean = ref nan and asymmetry_max = ref nan
          and asymmetry_at = ref ("", "") in
          if square then begin
            let total = ref 0. and pairs = ref 0 and worst = ref 0. in
            for i = 0 to rows - 1 do
              let above = matrix.Matrix.data.(i) in
              for j = i + 1 to rows - 1 do
                let gap =
                  Float.abs (Float.Array.get above j -. Float.Array.get matrix.Matrix.data.(j) i) in
                total := !total +. gap;
                incr pairs;
                if gap > !worst then begin
                  worst := gap;
                  asymmetry_at := names.(i), names.(j)
                end
              done
            done;
            asymmetry_max := !worst;
            asymmetry_mean := if !pairs = 0 then 0. else !total /. float_of_int !pairs
          end;
          { rows; columns; square; negative_cells = !negative_cells;
            diagonal_max = !diagonal_max; asymmetry_mean = !asymmetry_mean;
            asymmetry_max = !asymmetry_max; asymmetry_at = !asymmetry_at;
            (* A matrix with no off-diagonal cell at all -- one taxon -- has no
               extremes to report, and zero says that better than an infinity *)
            minimum = (if !minimum = infinity then 0. else !minimum);
            maximum = (if !maximum = neg_infinity then 0. else !maximum) }
      end
    let count_negative_branches t =
      let res = ref 0 in
      Newick.dfs_iter (fun _ _ -> ())
        (fun _ edge -> if Newick.get_edge_length edge < 0. then incr res)
        (fun _ _ -> ()) (fun _ _ -> ()) t;
      !res
    let total_branch_length t =
      let res = ref 0. in
      Newick.dfs_iter (fun _ _ -> ())
        (fun _ edge -> res := !res +. Newick.get_edge_length edge)
        (fun _ _ -> ()) (fun _ _ -> ()) t;
      !res
    let of_matrix ?(asymmetry = AsymmetryPolicy.Average)
                  ?(negative_branches = Newick.NegativeBranchesPolicy.OK) ?(verbose = false)
                  matrix =
      let names = matrix.Matrix.row_names and cols = matrix.Matrix.col_names in
      let n = Array.length names in
      (* What the matrix is, measured once.  The refusals below read [square]
         from here rather than testing it again, so that what this function
         requires and what [Statistics] reports can never come apart *)
      let stats = Statistics.of_matrix matrix in
      if n = 0 then
        Exception.raise_object_is_empty __FUNCTION__ "distance matrix";
      if Array.length cols <> n then
        Exception.raise __FUNCTION__ IO_Format
          (Printf.sprintf "A distance matrix must be square (found %d rows and %d columns)"
            n (Array.length cols));
      if not stats.Statistics.square then begin
        let i = ref 0 in
        while !i < n && cols.(!i) = names.(!i) do
          incr i
        done;
        Exception.raise __FUNCTION__ IO_Format
          (Printf.sprintf
            "A distance matrix must carry the same names on both axes and in the same order \
             (at position %d there is row '%s' but column '%s')" (!i + 1) names.(!i) cols.(!i))
      end;
      if stats.Statistics.asymmetry_max > 0. then begin
        let one, other = stats.Statistics.asymmetry_at in
        match asymmetry with
        | AsymmetryPolicy.Error ->
          Exception.raise __FUNCTION__ IO_Format
            (Printf.sprintf
              "The distance matrix is not symmetric: '%s' and '%s' disagree by %.10g"
              one other stats.Statistics.asymmetry_max)
        | AsymmetryPolicy.Average ->
          if verbose then
            Printf.eprintf
              "(%s): Averaging across the diagonal (largest disagreement %.10g, '%s' vs '%s')\n%!"
              __FUNCTION__ stats.Statistics.asymmetry_max one other
      end;
      (* A working copy, so that the caller's matrix is left alone and the
         asymmetry policy is applied once and for all.  The diagonal is never
         read by what follows, hence zeroed rather than believed *)
      let d = Array.init n (fun _ -> Float.Array.make n 0.) in
      for i = 0 to n - 1 do
        for j = i + 1 to n - 1 do
          let value =
            (Float.Array.get matrix.Matrix.data.(i) j
             +. Float.Array.get matrix.Matrix.data.(j) i) /. 2. in
          Float.Array.set d.(i) j value;
          Float.Array.set d.(j) i value
        done
      done;
      (* [node.(i)] is the subtree standing at active slot [i] and [r.(i)] the
         sum of its distances to every other active slot.  Slots are kept
         compact -- the active ones are always [0, m) -- so that every scan runs
         over contiguous memory *)
      let node = Array.init n (fun i -> Newick.leaf names.(i)) and r = Float.Array.make n 0. in
      for i = 0 to n - 1 do
        let sum = ref 0. in
        for j = 0 to n - 1 do
          sum := !sum +. Float.Array.get d.(i) j
        done;
        Float.Array.set r i !sum
      done;
      let negatives = ref 0 in
      let branch length =
        if length >= 0. then
          length
        else begin
          incr negatives;
          match negative_branches with
          | Newick.NegativeBranchesPolicy.OK -> length
          | Newick.NegativeBranchesPolicy.Zero -> 0.
          | Newick.NegativeBranchesPolicy.Error ->
            Exception.raise __FUNCTION__ IO_Format
              (Printf.sprintf
                "Neighbour joining produced a branch of negative length (%.10g), which means the \
                 distance matrix is not additive" length)
        end in
      let m = ref n and step = max 1 (n / 100) in
      if verbose then
        Printf.eprintf "(%s): Joining %d %s...\n%!"
          __FUNCTION__ n (String.pluralize_int ~plural:"taxa" "taxon" n);
      while !m > 3 do
        let active = !m in
        (* Studier and Keppler's Q, minimised.  The (m-2) scaling and the row
           sums are what keep the criterion consistent as the matrix shrinks *)
        let scale = float_of_int (active - 2) in
        let best = ref infinity and best_i = ref 0 and best_j = ref 1 in
        for i = 0 to active - 1 do
          let d_i = d.(i) and r_i = Float.Array.get r i in
          for j = i + 1 to active - 1 do
            let q = scale *. Float.Array.get d_i j -. r_i -. Float.Array.get r j in
            if q < !best then begin
              best := q;
              best_i := i;
              best_j := j
            end
          done
        done;
        let i = !best_i and j = !best_j in
        let d_ij = Float.Array.get d.(i) j in
        (* The two branch lengths add up to [d_ij] before the policy sees them,
           so a clamp shortens one branch and does not lengthen the other *)
        let length_i = d_ij /. 2. +. (Float.Array.get r i -. Float.Array.get r j) /. (2. *. scale) in
        let joined =
          Newick.join
            [| Newick.edge ~length:(branch length_i) (), node.(i);
               Newick.edge ~length:(branch (d_ij -. length_i)) (), node.(j) |] in
        (* The joined node takes slot [i].  Reading [d.(i).(k)] before writing it
           is safe because each [k] is touched once, and [k <> i] keeps the
           symmetric write out of the row being rewritten *)
        let sum = ref 0. in
        for k = 0 to active - 1 do
          if k <> i && k <> j then begin
            let d_ik = Float.Array.get d.(i) k and d_jk = Float.Array.get d.(j) k in
            let d_uk = (d_ik +. d_jk -. d_ij) /. 2. in
            Float.Array.set d.(i) k d_uk;
            Float.Array.set d.(k) i d_uk;
            Float.Array.set r k (Float.Array.get r k -. d_ik -. d_jk +. d_uk);
            sum := !sum +. d_uk
          end
        done;
        Float.Array.set d.(i) i 0.;
        Float.Array.set r i !sum;
        node.(i) <- joined;
        (* Slot [j] is retired by moving the last active slot into it *)
        let last = active - 1 in
        if j <> last then begin
          node.(j) <- node.(last);
          Float.Array.set r j (Float.Array.get r last);
          for k = 0 to last - 1 do
            let value = Float.Array.get d.(last) k in
            Float.Array.set d.(j) k value;
            Float.Array.set d.(k) j value
          done;
          Float.Array.set d.(j) j 0.
        end;
        decr m;
        if verbose && (n - !m) mod step = 0 then
          Printf.eprintf "%s\r(%s): Joined %d/%d nodes%!"
            String.TermIO.clear __FUNCTION__ (n - !m) (n - 3)
      done;
      if verbose then
        Printf.eprintf "%s\r(%s): Joined %d/%d nodes.\n%!"
          String.TermIO.clear __FUNCTION__ (n - !m) (max 0 (n - 3));
      if verbose && !negatives > 0 then
        Printf.eprintf "(%s): %d %s out negative (%s)\n%!"
          __FUNCTION__ !negatives
          (String.pluralize_int ~plural:"branches came" "branch came" !negatives)
          (match negative_branches with
           | Newick.NegativeBranchesPolicy.Zero -> "flattened to zero"
           | Newick.NegativeBranchesPolicy.OK | Newick.NegativeBranchesPolicy.Error -> "kept as they are");
      (* What is left resolves in closed form.  Three subtrees are an unrooted
         tree's natural top -- the trifurcation adds no bipartition of its own --
         and two are a single branch, which we halve so that neither leaf is
         arbitrarily privileged *)
      match !m with
      | 1 -> node.(0)
      | 2 ->
        let half = Float.Array.get d.(0) 1 /. 2. in
        Newick.join
          [| Newick.edge ~length:(branch half) (), node.(0);
             Newick.edge ~length:(branch half) (), node.(1) |]
      | _ ->
        let d_01 = Float.Array.get d.(0) 1 and d_02 = Float.Array.get d.(0) 2
        and d_12 = Float.Array.get d.(1) 2 in
        Newick.join
          [| Newick.edge ~length:(branch ((d_01 +. d_02 -. d_12) /. 2.)) (), node.(0);
             Newick.edge ~length:(branch ((d_01 +. d_12 -. d_02) /. 2.)) (), node.(1);
             Newick.edge ~length:(branch ((d_02 +. d_12 -. d_01) /. 2.)) (), node.(2) |]
  end

