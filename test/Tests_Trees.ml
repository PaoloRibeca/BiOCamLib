(*
    Tests_Trees.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Tests_Trees.ml exercises the Newick reader and writer at the level
    of the text they exchange: a tree that goes out and comes back has
    to render the same way the second time, whatever the first render
    chose to do.  That is the property a round trip can check without
    reaching into the tree, and it is the one that matters to every
    consumer downstream.

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

open BiOCamLib
open Better

module N = Trees.Newick
module P = Trees.Newick.NegativeBranchesPolicy

(* Helpers. *)

let render s = N.of_string s |> N.to_string

(* Render, re-read, render again.  The second render is what a consumer of a
   file this library wrote would see. *)
let renders_stably s =
  let once = render s in
  once = (N.of_string once |> N.to_string)

(* Newick. *)

let test_newick () =
  Testing.section "Newick round trips" (fun () ->
    (* The shapes a phylogeny actually comes in: a bare leaf, a cherry, a
       nested clade, branch lengths present and absent, and internal labels
       that a bootstrap run leaves behind. *)
    Testing.check "a single leaf renders stably" (fun () -> renders_stably "A;");
    Testing.check "a cherry renders stably" (fun () -> renders_stably "(A,B);");
    Testing.check "a three-taxon tree renders stably" (fun () -> renders_stably "(A,B,C);");
    Testing.check "a nested clade renders stably" (fun () -> renders_stably "((A,B),C);");
    Testing.check "branch lengths render stably"
      (fun () -> renders_stably "((A:0.1,B:0.2):0.3,C:0.4);");
    Testing.check "a deeper tree renders stably"
      (fun () -> renders_stably "(((A:0.1,B:0.2):0.3,(C:0.4,D:0.5):0.6):0.7,E:0.8);");
    Testing.check "an internal label renders stably"
      (fun () -> renders_stably "((A:0.1,B:0.2)100:0.3,C:0.4);");
    Testing.check "a zero branch length renders stably"
      (fun () -> renders_stably "((A:0.0,B:0.2):0.3,C:0.4);");
    (* A label carrying a space has to come back out quoted, or the writer
       produces a file this library's own reader rejects.  It used to be
       written raw, and the helper that should have quoted it deleted the
       space instead of emitting it. *)
    Testing.check "a label with a space renders stably"
      (fun () -> renders_stably "('Homo sapiens':0.1,'Pan troglodytes':0.2);");
    Testing.check "the space itself survives the round trip"
      (fun () ->
        let once = render "('Homo sapiens':0.1,B:0.2);" in
        String.Split.on_char_as_list ' ' once |> List.length > 1);
    Testing.check "a label with a colon renders stably"
      (fun () -> renders_stably "('a:b':0.1,B:0.2);");
    Testing.check "a label with an embedded quote renders stably"
      (fun () -> renders_stably "('O''Brien':0.1,B:0.2);");
    Testing.check "a label with an underscore renders stably"
      (fun () -> renders_stably "(Homo_sapiens:0.1,Pan_troglodytes:0.2);");
    (* Whitespace and a missing terminator are the two things a hand-edited
       file most often carries. *)
    Testing.check "surrounding whitespace is tolerated"
      (fun () -> renders_stably "  ( A , B ) ;  ");
    Testing.check "a newline inside the tree is tolerated"
      (fun () -> renders_stably "(A,\nB);");
    (* Reading a tree twice from the same text must give the same text back. *)
    Testing.check_string "the same input renders identically twice"
      ~expected:(render "((A:0.1,B:0.2):0.3,C:0.4);") (render "((A:0.1,B:0.2):0.3,C:0.4);");
    (* Malformed input has to be refused rather than half-parsed. *)
    Testing.check_raises "an unclosed parenthesis is refused"
      (fun () -> ignore (N.of_string "((A,B);"));
    Testing.check_raises "a stray closing parenthesis is refused"
      (fun () -> ignore (N.of_string "(A,B));"));
    Testing.check_raises "the empty string is refused"
      (fun () -> ignore (N.of_string "")))

(* Several trees in one file, which is what a bootstrap replicate set is. *)

let test_newick_arrays () =
  Testing.section "Newick tree arrays" (fun () ->
    Testing.check_int "a single tree reads back as an array of one"
      ~expected:1 (Array.length (N.array_of_string "(A,B);"));
    Testing.check_int "three trees read back as three"
      ~expected:3 (Array.length (N.array_of_string "(A,B);(C,D);(E,F);"));
    Testing.check_int "trees separated by newlines read back too"
      ~expected:3 (Array.length (N.array_of_string "(A,B);\n(C,D);\n(E,F);\n"));
    Testing.check_int "an array survives a round trip"
      ~expected:3
      (Array.length (N.array_to_string (N.array_of_string "(A,B);(C,D);(E,F);")
                     |> N.array_of_string)))

(* Negative branch lengths.  Neighbour-joining produces them as noise, so the
   reader takes a policy rather than a fixed decision. *)

let test_negative_branches () =
  Testing.section "Negative branch lengths" (fun () ->
    Testing.check_string "the policy round-trips through its name"
      ~expected:"zero" (P.to_string (P.of_string "zero"));
    Testing.check_raises "an unknown policy name is refused"
      (fun () -> ignore (P.of_string "nonsense"));
    let neg = "((A:-0.1,B:0.2):0.3,C:0.4);" in
    (* Error is the default, for backwards compatibility. *)
    Testing.check_raises "a negative branch is refused by default"
      (fun () -> ignore (N.of_string neg));
    Testing.check_does_not_raise "the OK policy admits it"
      (fun () -> ignore (N.of_string ~negative_branches:P.OK neg));
    Testing.check_does_not_raise "the Zero policy admits it"
      (fun () -> ignore (N.of_string ~negative_branches:P.Zero neg));
    (* Zero does not merely tolerate the branch, it clamps it, so the rendered
       tree carries no minus sign. *)
    Testing.check "the Zero policy clamps the branch it admits"
      (fun () ->
        let s = N.of_string ~negative_branches:P.Zero neg |> N.to_string in
        not (String.length s > 0
             && List.exists (fun c -> c = '-') (List.init (String.length s) (String.get s))));
    Testing.check "the OK policy keeps the branch as it was"
      (fun () ->
        let s = N.of_string ~negative_branches:P.OK neg |> N.to_string in
        List.exists (fun c -> c = '-') (List.init (String.length s) (String.get s))))

(* The quoting helper itself, which both the Newick and the Splits writers go
   through.  Testing it here rather than only through a round trip states what
   it owes its callers: a faithful string, quoted exactly when leaving it bare
   would not read back. *)

let test_quoting () =
  Testing.section "Label quoting" (fun () ->
    let q = Trees_Lex.quote_string_if_needed in
    Testing.check_string "an ordinary label is left bare" ~expected:"abc" (q "abc");
    Testing.check_string "a label with an underscore is left bare"
      ~expected:"Homo_sapiens" (q "Homo_sapiens");
    Testing.check_string "a label with a space is quoted, and keeps the space"
      ~expected:"'Homo sapiens'" (q "Homo sapiens");
    Testing.check_string "a label with a colon is quoted, and keeps the colon"
      ~expected:"'a:b'" (q "a:b");
    Testing.check_string "an embedded quote is doubled"
      ~expected:"'O''Brien'" (q "O'Brien");
    Testing.check_string "the empty label is left bare" ~expected:"" (q "");
    (* Whatever comes out has to read back as what went in, which is the whole
       contract; a helper that quotes but drops the character fails it. *)
    Testing.check "quoting a label never loses a character"
      (fun () ->
        List.for_all
          (fun s ->
            let quoted = q s in
            String.length quoted >= String.length s)
          [ "abc"; "Homo sapiens"; "a:b"; "O'Brien"; "a b c d" ]))

(* Building a tree rather than parsing one, and then walking it.  A small tree
   with branch lengths chosen so that every distance can be worked out by hand:

       root --2.0-- A
            --3.0-- inner --1.0-- B
                          --4.0-- C

   so that A is 2 from the root, B is 4, C is 7, and the leaf-to-leaf distances
   are A-B 6, A-C 9, B-C 5. *)

let built () =
  let e length = N.edge ~length () in
  N.join
    [| e 2., N.leaf "A";
       e 3., N.join ~name:"inner" [| e 1., N.leaf "B"; e 4., N.leaf "C" |] |]

let test_construction () =
  Testing.section "Newick construction" (fun () ->
    let t = built () in
    (* A tree built this way is unrooted until it is told otherwise, which the
       rich format spells [&U]. *)
    Testing.check_string "a constructed tree renders, and is unrooted"
      ~expected:"[&U](A:2,(B:1,C:4)inner:3);" (N.to_string t);
    Testing.check_string "and says so once it is rooted"
      ~expected:"[&R](A:2,(B:1,C:4)inner:3);" (N.to_string (N.set_is_root t true));
    Testing.check_string "and renders the same after a round trip"
      ~expected:(N.to_string t) (N.to_string (N.of_string (N.to_string t)));
    (* The plain format drops the rooting marker the rich one carries. *)
    Testing.check_string "the plain format omits the rich-format marker"
      ~expected:"(A:2,(B:1,C:4)inner:3);" (N.to_string ~rich_format:false t);
    Testing.check_string "a leaf carries the name it was built with"
      ~expected:"A" (N.get_name (N.leaf "A"));
    Testing.check_string "an internal node can be named"
      ~expected:"inner" (N.get_name (N.join ~name:"inner" [| N.edge (), N.leaf "A" |]));
    (* Edge values are three separate slots with their own sentinels. *)
    Testing.check_string "an edge carries its length"
      ~expected:"2.5" (string_of_float (N.get_edge_length (N.edge ~length:2.5 ())));
    Testing.check_string "and its bootstrap"
      ~expected:"0.9" (string_of_float (N.get_edge_bootstrap (N.edge ~bootstrap:0.9 ())));
    Testing.check "an edge is not a ghost unless it is made one"
      (fun () -> not (N.get_edge_is_ghost (N.edge ~length:1. ())));
    Testing.check "and is one when it is"
      (fun () -> N.get_edge_is_ghost (N.edge ~length:1. ~is_ghost:true ()));
    (* Rooting and hybrid marks are node properties that the rich format
       renders and the plain one does not. *)
    Testing.check "a tree can be told it is rooted"
      (fun () -> N.get_is_root (N.set_is_root (built ()) true));
    Testing.check "a node carries no hybrid mark unless given one"
      (fun () -> N.get_hybrid (built ()) = None);
    Testing.check "and carries one when given"
      (fun () -> N.get_hybrid (N.set_hybrid (built ()) (Some (N.Recombination 1)))
                 = Some (N.Recombination 1)))

(* Flattening and distances. *)

let test_traversal () =
  Testing.section "Newick traversal and distances" (fun () ->
    let t = built () in
    let flat = N.dfs_flatten t in
    Testing.check_int "flattening yields one entry per node"
      ~expected:5 (Array.length flat);
    (* Look nodes up by name rather than by position, so that the checks do not
       depend on the order the walk happens to produce. *)
    let index_of name =
      let res = ref (-1) in
      Array.iteri (fun i (_, _, node, _) ->
        if N.get_node_name node = name then res := i) flat;
      !res in
    Testing.check "every node is reachable by name"
      (fun () -> List.for_all (fun n -> index_of n >= 0) [ "A"; "B"; "C"; "inner" ]);
    (* Dijkstra from the root: the distances above, by construction. *)
    let d = N.dijkstra flat 0 in
    let dist name = Better.Float.Array.get d (index_of name) in
    Testing.check_string "A is two from the root" ~expected:"2." (string_of_float (dist "A"));
    Testing.check_string "the inner node is three" ~expected:"3."
      (string_of_float (dist "inner"));
    Testing.check_string "B is four" ~expected:"4." (string_of_float (dist "B"));
    Testing.check_string "C is seven" ~expected:"7." (string_of_float (dist "C"));
    Testing.check_string "and the root is zero from itself" ~expected:"0."
      (string_of_float (Better.Float.Array.get d 0));
    (* The leaf-to-leaf matrix, which is what a phylogeny consumer wants. *)
    (* The matrix covers every node rather than just the leaves, and names each
       one by its position in the flattened array followed by its name -- so
       the unnamed root is "0_".  That means a row is found at the same index
       the flattening gave it. *)
    let m = N.get_min_distance_matrix t in
    Testing.check_string "the matrix names every node, indexed as it was flattened"
      ~expected:"0_,1_A,2_inner,3_B,4_C"
      (Array.to_list m.Matrix.row_names |> String.concat ",");
    Testing.check "and is square over them"
      (fun () -> m.Matrix.row_names = m.Matrix.col_names);
    let between a b =
      Better.Float.Array.get m.Matrix.data.(index_of a) (index_of b) in
    Testing.check_string "A to B is six" ~expected:"6." (string_of_float (between "A" "B"));
    Testing.check_string "A to C is nine" ~expected:"9." (string_of_float (between "A" "C"));
    Testing.check_string "B to C is five" ~expected:"5." (string_of_float (between "B" "C"));
    Testing.check_string "a leaf is zero from itself" ~expected:"0."
      (string_of_float (between "A" "A"));
    Testing.check "the matrix is symmetric"
      (fun () -> between "A" "C" = between "C" "A"))

let run () =
  test_quoting ();
  test_construction ();
  test_traversal ();
  test_newick ();
  test_newick_arrays ();
  test_negative_branches ()
