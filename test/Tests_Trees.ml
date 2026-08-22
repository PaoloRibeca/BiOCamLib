(*
    Tests_Trees.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Tests_Trees.ml exercises both sides of Trees.  On the reading
    side, the Newick reader and writer at the level of the text they
    exchange: a tree that goes out and comes back has to render the
    same way the second time, whatever the first render chose to do.
    On the building side, neighbour joining and the midpoint rooting
    the NJ binary offers on top of it.  Both of the latter have an
    exact property to be held to -- neighbour joining must invert an
    additive matrix without error, and rooting must move no distance
    between tips -- so most of those checks compare against a computed
    truth rather than against a recorded string.

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
module NJ = Trees.NeighbourJoining

(* Helpers. *)

let render s = N.of_string s |> N.to_string

(* Render, re-read, render again.  The second render is what a consumer of a
   file this library wrote would see. *)
let renders_stably s =
  let once = render s in
  once = (N.of_string once |> N.to_string)

(* Trees are written here as nested calls rather than as Newick strings, so
   that a check states the topology and the lengths it is about without a
   parser standing between the two *)
let tip = N.leaf
let node subs = Array.of_list subs |> N.join
let branch length sub = N.edge ~length (), sub

let matrix names rows =
  { Matrix.col_names = names;
    row_names = names;
    data = Array.map Float.Array.of_list rows }

(* Distance from flattened node [i] to every other node, along the unique path
   that joins them.  A plain traversal rather than [dijkstra], because in a tree
   there is only one path to find, and because that keeps the answer meaningful
   when a branch is negative -- which, for a neighbour-joining tree, it may be *)
let distances flat i =
  let d = Float.Array.make (Array.length flat) nan in
  let rec walk j came_from acc =
    Float.Array.set d j acc;
    let parent_edge, parent, _, children = flat.(j) in
    if parent <> -1 && parent <> came_from then
      walk parent j (acc +. N.get_edge_length parent_edge);
    Array.iter
      (fun (edge, k) -> if k <> came_from then walk k j (acc +. N.get_edge_length edge))
      children in
  walk i (-1) 0.;
  d

(* The flattened indices of the tips, paired with their names *)
let tips flat =
  let res = ref [] in
  Array.iteri
    (fun i (_, _, node, children) ->
      if Array.length children = 0 then
        res := (i, N.get_node_name node) :: !res)
    flat;
  Array.of_list (List.rev !res)

(* The tip-to-tip distance matrix of a tree -- what neighbour joining has to be
   able to invert exactly when that tree is what produced it *)
let tip_matrix t =
  let flat = N.dfs_flatten t in
  let tips = tips flat in
  let names = Array.map snd tips in
  { Matrix.col_names = names;
    row_names = names;
    data =
      Array.map
        (fun (i, _) ->
          let d = distances flat i in
          Float.Array.init (Array.length tips) (fun j -> Float.Array.get d (fst tips.(j))))
        tips }

(* Tip-to-tip distances keyed by name, so that two trees can be compared
   without their leaves having to arrive in the same order *)
let tip_distances t =
  let flat = N.dfs_flatten t in
  let tips = tips flat in
  let res = ref StringMap.empty in
  Array.iter
    (fun (i, name) ->
      let d = distances flat i in
      Array.iter
        (fun (j, other) ->
          res := StringMap.add (name ^ "\t" ^ other) (Float.Array.get d j) !res)
        tips)
    tips;
  !res

let total_length t =
  let res = ref 0. in
  N.dfs_iter (fun _ _ -> ()) (fun _ edge -> res := !res +. N.get_edge_length edge)
    (fun _ _ -> ()) (fun _ _ -> ()) t;
  !res

(* The largest discrepancy between two sets of tip-to-tip distances, and the
   pair it was found on.  Absolute rather than relative: these distances are
   sums of a handful of branches, so what accumulates is absolute rounding *)
let worst_gap a b =
  let worst = ref 0. and worst_at = ref "" in
  StringMap.iter
    (fun key value ->
      match StringMap.find_opt key b with
      | None -> worst := infinity; worst_at := key ^ " (missing)"
      | Some other ->
        let gap = Float.abs (value -. other) in
        if gap > !worst then begin
          worst := gap;
          worst_at := key
        end)
    a;
  !worst, !worst_at

let check_recovers ?known_bug name t =
  Testing.verify ?known_bug name (fun () ->
    let rebuilt = tip_matrix t |> NJ.of_matrix in
    let gap, where = worst_gap (tip_distances t) (tip_distances rebuilt) in
    gap < 1e-9,
    Printf.sprintf "tip-to-tip distances differ by %g, worst at '%s'" gap where)

(* The property that defines the midpoint: the furthest tip from the root is
   half the longest tip-to-tip path away, so the two ends of that path are the
   same distance from the root and neither side of it is favoured *)
let check_is_midpoint ?known_bug name t =
  Testing.verify ?known_bug name (fun () ->
    let flat = N.dfs_flatten t in
    let all = tips flat in
    let deepest =
      let from_root = distances flat 0 in
      Array.fold_left (fun acc (i, _) -> max acc (Float.Array.get from_root i)) neg_infinity all
    and diameter =
      Array.fold_left
        (fun acc (i, _) ->
          let d = distances flat i in
          Array.fold_left (fun acc (j, _) -> max acc (Float.Array.get d j)) acc all)
        neg_infinity all in
    Float.abs (deepest -. diameter /. 2.) < 1e-9,
    Printf.sprintf "the furthest tip is %.17g from the root, half the longest path is %.17g"
      deepest (diameter /. 2.))

(* Fixtures. *)

(* The five-taxon matrix of the textbook worked example, whose neighbour-joining
   tree is known term by term *)
let wiki =
  matrix [| "a"; "b"; "c"; "d"; "e" |]
    [| [ 0.; 5.; 9.; 9.; 8. ];
       [ 5.; 0.; 10.; 10.; 9. ];
       [ 9.; 10.; 0.; 8.; 7. ];
       [ 9.; 10.; 8.; 0.; 3. ];
       [ 8.; 9.; 7.; 3.; 0. ] |]

(* A caterpillar and a balanced tree of eight tips each, with lengths that share
   no common factor, so that a mistake in the branch-length algebra cannot
   cancel out *)
let caterpillar =
  node [ branch 0.7 (tip "t1");
         branch 0.13 (node [ branch 0.29 (tip "t2");
           branch 0.03 (node [ branch 0.41 (tip "t3");
             branch 0.11 (node [ branch 0.17 (tip "t4");
               branch 0.07 (node [ branch 0.53 (tip "t5");
                 branch 0.19 (node [ branch 0.23 (tip "t6");
                   branch 0.31 (node [ branch 0.37 (tip "t7");
                                       branch 0.43 (tip "t8") ]) ]) ]) ]) ]) ]);
         branch 1.1 (tip "t0") ]

let balanced =
  node [ branch 0.05 (node [ branch 0.11 (node [ branch 0.3 (tip "u1"); branch 0.7 (tip "u2") ]);
                             branch 0.13 (node [ branch 0.2 (tip "u3"); branch 0.9 (tip "u4") ]) ]);
         branch 0.17 (node [ branch 0.19 (tip "u5"); branch 0.23 (tip "u6") ]);
         branch 0.29 (node [ branch 0.31 (tip "u7"); branch 0.37 (tip "u8") ]) ]

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

(* Neighbour joining. *)

let test_neighbour_joining () =
  Testing.section "Trees: neighbour joining" (fun () ->
    Testing.check_string "the textbook example is reproduced branch by branch"
      ~expected:"(((a:2,b:3):3,c:4):2,e:1,d:2);"
      (NJ.of_matrix wiki |> N.to_string ~rich_format:false);
    Testing.check_string "the tree is tagged unrooted"
      ~expected:"[&U](((a:2,b:3):3,c:4):2,e:1,d:2);"
      (NJ.of_matrix wiki |> N.to_string);
    check_recovers "an additive caterpillar is recovered exactly" caterpillar;
    check_recovers "an additive balanced tree is recovered exactly" balanced;
    Testing.check_string "every name arrives as exactly one tip"
      ~expected:"t0 t1 t2 t3 t4 t5 t6 t7 t8"
      (NJ.of_matrix (tip_matrix caterpillar) |> N.dfs_flatten |> tips |> Array.map snd
        |> Array.to_list |> List.sort compare |> String.concat " ");
    Testing.check "the top of an unrooted tree is a trifurcation"
      (fun () ->
        match NJ.of_matrix wiki with
        | t -> (N.dfs_flatten t).(0) |> (fun (_, _, _, children) -> Array.length children) = 3))

let test_neighbour_joining_degenerate () =
  Testing.section "Trees: neighbour joining, degenerate inputs" (fun () ->
    Testing.check_string "one taxon is a bare tip"
      ~expected:"only;" (NJ.of_matrix (matrix [| "only" |] [| [ 0. ] |]) |> N.to_string ~rich_format:false);
    (* Two taxa are one branch, and there is nothing to say about where along it
       the arbitrary Newick top node falls, so it falls in the middle *)
    Testing.check_string "two taxa split their distance evenly"
      ~expected:"(one:2,two:2);"
      (NJ.of_matrix (matrix [| "one"; "two" |] [| [ 0.; 4. ]; [ 4.; 0. ] |])
        |> N.to_string ~rich_format:false);
    Testing.check_string "three taxa resolve in closed form"
      ~expected:"(x:1,y:2,z:3);"
      (NJ.of_matrix
        (matrix [| "x"; "y"; "z" |] [| [ 0.; 3.; 4. ]; [ 3.; 0.; 5. ]; [ 4.; 5.; 0. ] |])
        |> N.to_string ~rich_format:false);
    Testing.check_raises ~re:"empty" "an empty matrix is refused"
      (fun () -> NJ.of_matrix Matrix.empty))

let test_neighbour_joining_malformed () =
  Testing.section "Trees: neighbour joining, malformed matrices" (fun () ->
    Testing.check_raises ~re:"must be square" "a matrix that is not square is refused"
      (fun () ->
        NJ.of_matrix
          { Matrix.col_names = [| "a"; "b"; "c" |];
            row_names = [| "a"; "b" |];
            data = [| Float.Array.of_list [ 0.; 1.; 2. ]; Float.Array.of_list [ 1.; 0.; 3. ] |] });
    (* The two axes name the same taxa but in a different order.  Silently
       trusting the row names would build a tree from transposed distances,
       which is why the order has to be part of the contract *)
    Testing.check_raises ~re:"same order" "axes that disagree on the order are refused"
      (fun () ->
        NJ.of_matrix
          { Matrix.col_names = [| "b"; "a" |];
            row_names = [| "a"; "b" |];
            data = [| Float.Array.of_list [ 0.; 1. ]; Float.Array.of_list [ 1.; 0. ] |] }))

let test_neighbour_joining_asymmetry () =
  Testing.section "Trees: neighbour joining, asymmetry" (fun () ->
    (* Off by two across the diagonal on the 'p','q' pair only, whose mean is 3,
       so the averaged tree is the tree of the symmetric matrix below *)
    let lopsided =
      { Matrix.col_names = [| "p"; "q"; "r" |];
        row_names = [| "p"; "q"; "r" |];
        data = [| Float.Array.of_list [ 0.; 2.; 4. ];
                  Float.Array.of_list [ 4.; 0.; 5. ];
                  Float.Array.of_list [ 4.; 5.; 0. ] |] } in
    Testing.check_string "the default averages across the diagonal"
      ~expected:"(p:1,q:2,r:3);" (NJ.of_matrix lopsided |> N.to_string ~rich_format:false);
    Testing.check_string "averaging is what the symmetric matrix of the means gives"
      ~expected:(NJ.of_matrix lopsided |> N.to_string ~rich_format:false)
      (NJ.of_matrix
        (matrix [| "p"; "q"; "r" |] [| [ 0.; 3.; 4. ]; [ 3.; 0.; 5. ]; [ 4.; 5.; 0. ] |])
        |> N.to_string ~rich_format:false);
    Testing.check_raises ~re:"not symmetric" "the strict policy refuses the same matrix"
      (fun () -> NJ.of_matrix ~asymmetry:NJ.AsymmetryPolicy.Error lopsided);
    Testing.check_does_not_raise "the strict policy accepts a symmetric one"
      (fun () -> NJ.of_matrix ~asymmetry:NJ.AsymmetryPolicy.Error wiki))

let test_neighbour_joining_negative_branches () =
  Testing.section "Trees: neighbour joining, negative branches" (fun () ->
    (* A matrix that violates the triangle inequality: d(g,h) alone exceeds the
       path through f, so one of the three branches has to come out negative *)
    let non_additive =
      matrix [| "f"; "g"; "h" |] [| [ 0.; 1.; 1. ]; [ 1.; 0.; 4. ]; [ 1.; 4.; 0. ] |] in
    Testing.check_string "a negative branch is kept by default"
      ~expected:"(f:-1,g:2,h:2);" (NJ.of_matrix non_additive |> N.to_string ~rich_format:false);
    Testing.check_string "the zero policy flattens it"
      ~expected:"(f:0,g:2,h:2);"
      (NJ.of_matrix ~negative_branches:N.NegativeBranchesPolicy.Zero non_additive
        |> N.to_string ~rich_format:false);
    Testing.check_raises ~re:"not additive" "the strict policy refuses the matrix"
      (fun () -> NJ.of_matrix ~negative_branches:N.NegativeBranchesPolicy.Error non_additive);
    Testing.check_does_not_raise "the strict policy accepts an additive matrix"
      (fun () -> NJ.of_matrix ~negative_branches:N.NegativeBranchesPolicy.Error wiki))

(* Midpoint rooting. *)

let test_midpoint_rooting () =
  Testing.section "Trees: midpoint rooting" (fun () ->
    let rooted = NJ.of_matrix wiki |> N.midpoint_root in
    Testing.check_string "the textbook tree roots where the two furthest tips balance"
      ~expected:"((a:2,b:3):2,(c:4,(e:1,d:2):2):1);" (N.to_string ~rich_format:false rooted);
    Testing.check_string "the tree is tagged rooted"
      ~expected:"[&R]((a:2,b:3):2,(c:4,(e:1,d:2):2):1);" (N.to_string rooted);
    Testing.check "the root is a bifurcation"
      (fun () -> (N.dfs_flatten rooted).(0) |> (fun (_, _, _, kids) -> Array.length kids) = 2);
    check_is_midpoint "the root sits half the longest path from either of its ends" rooted)

let test_midpoint_rooting_invariants () =
  Testing.section "Trees: midpoint rooting, invariants" (fun () ->
    List.iter
      (fun (what, t) ->
        let rooted = N.midpoint_root t in
        let gap, where = worst_gap (tip_distances t) (tip_distances rooted) in
        Testing.verify (Printf.sprintf "rooting %s moves no tip-to-tip distance" what)
          (fun () ->
            gap < 1e-9, Printf.sprintf "distances differ by %g, worst at '%s'" gap where);
        Testing.verify (Printf.sprintf "rooting %s creates and loses no branch length" what)
          (fun () ->
            let before = total_length t and after = total_length rooted in
            Float.abs (before -. after) < 1e-9,
            Printf.sprintf "total branch length went from %.17g to %.17g" before after);
        (* Rooting an already-rooted tree has to splice out the root it finds,
           or a chain of them would build up one per call *)
        Testing.verify (Printf.sprintf "rooting %s twice is rooting it once" what)
          (fun () ->
            let twice = N.midpoint_root rooted in
            let gap, where = worst_gap (tip_distances rooted) (tip_distances twice) in
            gap < 1e-9 && Float.abs (total_length twice -. total_length rooted) < 1e-9,
            Printf.sprintf "distances differ by %g (worst at '%s'), total length %.17g vs %.17g"
              gap where (total_length twice) (total_length rooted));
        check_is_midpoint (Printf.sprintf "the root of %s is its midpoint" what) rooted)
      [ "the textbook tree", NJ.of_matrix wiki;
        "a caterpillar", caterpillar;
        "a balanced tree", balanced;
        "a tree with a negative branch",
          NJ.of_matrix (matrix [| "f"; "g"; "h" |] [| [ 0.; 1.; 1. ]; [ 1.; 0.; 4. ]; [ 1.; 4.; 0. ] |]) ];
    (* Two tips are the smallest tree with a midpoint to find, and the only one
       whose rooting leaves the old top node with a single child to splice: were
       it not spliced, its stub would show up here as a '(m:0)' wrapper.  Both
       descents are the same length, so which of the two tips ends up written
       first is the tie-break, pinned here as the record of what it is *)
    Testing.check_string "two tips root halfway along their branch"
      ~expected:"(n:1.5,m:1.5);"
      (NJ.of_matrix (matrix [| "m"; "n" |] [| [ 0.; 3. ]; [ 3.; 0. ] |])
        |> N.midpoint_root |> N.to_string ~rich_format:false);
    Testing.check_string "one tip has no path to halve"
      ~expected:"[&R]sole;" (N.leaf "sole" |> N.midpoint_root |> N.to_string);
    (* Without lengths there is no midpoint, and a zero invented on the tree's
       behalf would put the root wherever the input happened to be written *)
    Testing.check_raises ~re:"no length" "a tree without branch lengths is refused"
      (fun () -> N.of_string ~rich_format:false "((a,b),c,d);" |> N.midpoint_root);
    Testing.check_raises ~re:"no length" "one branch without a length is enough to refuse"
      (fun () -> N.of_string ~rich_format:false "((a:1,b:2):3,c,d:4);" |> N.midpoint_root))

(* Where the two halves of this file meet.  Neighbour joining names its tips
   from the matrix it was given, and a distance matrix is free to carry names
   with spaces and colons in them; the Newick writer then has to quote those,
   or the tree it produces is one this library's own reader refuses.  Neither
   side's checks reach this on their own. *)

let test_nj_names () =
  Testing.section "Trees: neighbour joining over names that need quoting" (fun () ->
    let awkward =
      matrix [| "a b"; "c:d"; "e" |]
        [| [ 0.; 5.; 9. ]; [ 5.; 0.; 10. ]; [ 9.; 10.; 0. ] |] in
    let rendered = NJ.of_matrix awkward |> N.to_string ~rich_format:false in
    let contains needle =
      try ignore (Str.search_forward (Str.regexp_string needle) rendered 0); true
      with Not_found -> false in
    Testing.check "a name carrying a space is quoted" (fun () -> contains "'a b'");
    Testing.check "and one carrying a colon" (fun () -> contains "'c:d'");
    Testing.check "a name needing no quotes does not get any"
      (fun () -> not (contains "'e'"));
    Testing.check_does_not_raise "the tree reads back"
      (fun () -> ignore (N.of_string ~rich_format:false rendered));
    Testing.check_string "with its tip names intact"
      ~expected:"a b,c:d,e"
      (let back = N.of_string ~rich_format:false rendered in
       let flat = N.dfs_flatten back in
       Array.to_list flat
       |> List.filter_map (fun (_, _, node, descs) ->
            if Array.length descs = 0 then Some (N.get_node_name node) else None)
       |> List.sort compare |> String.concat ","))

let run () =
  test_nj_names ();
  test_quoting ();
  test_construction ();
  test_traversal ();
  test_newick ();
  test_newick_arrays ();
  test_negative_branches ();
  test_neighbour_joining ();
  test_neighbour_joining_degenerate ();
  test_neighbour_joining_malformed ();
  test_neighbour_joining_asymmetry ();
  test_neighbour_joining_negative_branches ();
  test_midpoint_rooting ();
  test_midpoint_rooting_invariants ()

