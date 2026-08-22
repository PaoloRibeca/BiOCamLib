(*
    Tests_Tools.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Tests_Tools.ml exercises the general-purpose containers in Tools:
    the array-backed stack, the prefix trie behind command-line option
    matching, and the multimap.  The stack's [pop_n] is pinned here in
    particular: it drops n elements and returns the last one dropped,
    generalising [pop] rather than returning the n of them, and the one
    caller in the wider codebase depends on exactly that.

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

module S = Tools.ArrayStack
module MM = Tools.Multimap (ComparableInt) (ComparableInt)
module TC = Tools.TransitiveClosure.Make (ComparableInt)

(* Helpers. *)

(* A stack holding 1..n, pushed in ascending order, so that the bottom is 1. *)
let stack_of_range n =
  let s = S.empty () in
  for i = 1 to n do
    S.push s i
  done;
  s

let ints_to_string l = List.map string_of_int l |> String.concat ","

let array_to_string a = Array.to_list a |> ints_to_string

(* The array-backed stack. *)

let test_arraystack () =
  Testing.section "ArrayStack" (fun () ->
    Testing.check "a fresh stack is empty" (fun () -> S.is_empty (S.empty ()));
    Testing.check_int "an empty stack has length zero" ~expected:0 (S.length (S.empty ()));
    let s = stack_of_range 3 in
    Testing.check_int "pushing three elements gives length three" ~expected:3 (S.length s);
    Testing.check "a stack with elements is not empty" (fun () -> not (S.is_empty s));
    Testing.check_int "top returns the last pushed element" ~expected:3 (S.top s);
    Testing.check_int "top does not remove it" ~expected:3 (S.length s);
    Testing.check_int "pop returns the last pushed element" ~expected:3 (S.pop s);
    Testing.check_int "pop removes it" ~expected:2 (S.length s);
    Testing.check_int "and the next pop returns the one below" ~expected:2 (S.pop s);
    (* The option-returning variants are the only safe way to drain a stack. *)
    Testing.check "pop_opt returns None on an empty stack"
      (fun () -> S.pop_opt (S.empty ()) = None);
    Testing.check "top_opt returns None on an empty stack"
      (fun () -> S.top_opt (S.empty ()) = None);
    Testing.check_raises "pop raises on an empty stack" (fun () -> ignore (S.pop (S.empty ())));
    Testing.check_raises "top raises on an empty stack" (fun () -> ignore (S.top (S.empty ())));
    (* [pop_n] drops n and returns the LAST one dropped, which for n = 1 is
       exactly [pop].  It is not a bulk pop: NINJA's backtracking assembler
       uses the returned value directly as a truncation length. *)
    Testing.check_int "pop_n with n = 1 agrees with pop"
      ~expected:(S.pop (stack_of_range 5)) (S.pop_n (stack_of_range 5) 1);
    Testing.check_int "pop_n returns the deepest of the elements it dropped"
      ~expected:3 (S.pop_n (stack_of_range 5) 3);
    Testing.check_int "pop_n leaves the stack shortened by n"
      ~expected:2 (let s = stack_of_range 5 in ignore (S.pop_n s 3); S.length s);
    Testing.check_raises "pop_n raises when asked for more than there is"
      (fun () -> ignore (S.pop_n (stack_of_range 2) 3));
    Testing.check_int "pop_n may take the whole stack"
      ~expected:1 (S.pop_n (stack_of_range 5) 5);
    (* Bulk pushes. *)
    Testing.check_string "push_array appends in array order"
      ~expected:"1,2,3"
      (let s = S.empty () in
       S.push_array s [| 1; 2; 3 |];
       array_to_string (S.contents s));
    Testing.check_string "push_arraystack appends another stack's contents"
      ~expected:"1,2,3,1,2"
      (let s = stack_of_range 3 in
       S.push_arraystack s (stack_of_range 2);
       array_to_string (S.contents s));
    (* contents is bottom-to-top; rcontents is its reverse. *)
    Testing.check_string "contents runs from the bottom up"
      ~expected:"1,2,3" (array_to_string (S.contents (stack_of_range 3)));
    Testing.check_string "rcontents runs from the top down"
      ~expected:"3,2,1" (array_to_string (S.rcontents (stack_of_range 3)));
    (* Indexed access is in the same frame as contents. *)
    Testing.check_int "get indexes from the bottom" ~expected:1 (S.get (stack_of_range 3) 0);
    Testing.check_int "the indexing operator agrees with get"
      ~expected:1 ((stack_of_range 3).S.@(0));
    Testing.check_string "set replaces in place"
      ~expected:"1,9,3"
      (let s = stack_of_range 3 in
       S.set s 1 9;
       array_to_string (S.contents s));
    Testing.check_raises "get past the top is refused"
      (fun () -> ignore (S.get (stack_of_range 3) 3));
    (* iter walks from the top down, riter from the bottom up -- the opposite
       way round from contents, which is laid out bottom-first. *)
    Testing.check_string "iter walks from the top down"
      ~expected:"3,2,1"
      (let acc = ref [] in
       S.iter (fun i -> List.accum acc i) (stack_of_range 3);
       ints_to_string (List.rev !acc));
    Testing.check_string "riter walks from the bottom up"
      ~expected:"1,2,3"
      (let acc = ref [] in
       S.riter (fun i -> List.accum acc i) (stack_of_range 3);
       ints_to_string (List.rev !acc));
    Testing.check_string "riter agrees with contents"
      ~expected:(array_to_string (S.contents (stack_of_range 3)))
      (let acc = ref [] in
       S.riter (fun i -> List.accum acc i) (stack_of_range 3);
       ints_to_string (List.rev !acc));
    (* Emptying and copying. *)
    Testing.check "clear empties the stack"
      (fun () -> let s = stack_of_range 3 in S.clear s; S.is_empty s);
    Testing.check "reset empties the stack"
      (fun () -> let s = stack_of_range 3 in S.reset s; S.is_empty s);
    Testing.check_string "a copy does not share storage with its original"
      ~expected:"1,2,3"
      (let s = stack_of_range 3 in
       let c = S.copy s in
       S.push s 4;
       array_to_string (S.contents c)))

(* The prefix trie.  This is what resolves an abbreviated command-line option,
   so the distinction between "no match", "one longer match" and "several"
   is the whole point. *)

let test_trie () =
  Testing.section "Trie" (fun () ->
    let t =
      List.fold_left Tools.Trie.add (Tools.Trie.create ())
        [ "annotation"; "annotate"; "brief" ] in
    Testing.check_int "the trie holds every string added" ~expected:3 (Tools.Trie.length t);
    Testing.check_string "all returns them"
      ~expected:"annotate,annotation,brief"
      (Tools.Trie.all t |> Array.to_list |> List.sort compare |> String.concat ",");
    Testing.check_int "a string added twice is stored once"
      ~expected:3 (Tools.Trie.length (Tools.Trie.add t "brief"));
    (* Re-adding a string used to graft the root in as a child of the node above
       the last character, making the trie cyclic; every later walk down that
       branch then recursed until the stack gave out.  The lookups below are the
       ones that used to overflow, so they double as the regression check. *)
    Testing.check_does_not_raise "re-adding a string leaves the trie walkable"
      (fun () ->
        let t = Tools.Trie.add t "brief" in
        ignore (Tools.Trie.find_present t "brief");
        ignore (Tools.Trie.all t));
    Testing.check "and leaves it holding the same strings"
      (fun () ->
        let t = Tools.Trie.add t "brief" in
        Tools.Trie.all t |> Array.to_list |> List.sort compare
          = [ "annotate"; "annotation"; "brief" ]);
    (* An exact entry is present and unambiguous. *)
    Testing.check "an exact match is present"
      (fun () -> Tools.Trie.find_present t "brief" <> None);
    Testing.check "an exact match is unambiguous"
      (fun () -> Tools.Trie.find_unambiguous t "brief" <> None);
    (* A prefix shared by two entries is not present, and resolves to neither. *)
    Testing.check "a shared prefix is not itself present"
      (fun () -> Tools.Trie.find_present t "anno" = None);
    Testing.check "a shared prefix is ambiguous"
      (fun () -> Tools.Trie.find_unambiguous t "anno" = None);
    (* A prefix of exactly one entry resolves to it. *)
    Testing.check "a prefix of a single entry is unambiguous"
      (fun () -> Tools.Trie.find_unambiguous t "br" <> None);
    Testing.check "a string that is not a prefix of anything is absent"
      (fun () -> Tools.Trie.find_present t "zzz" = None);
    Testing.check_int "longest_prefix measures how far a query stays in the trie"
      ~expected:5 (Tools.Trie.longest_prefix t "brief-and-then-some");
    Testing.check_int "longest_prefix is zero when nothing matches"
      ~expected:0 (Tools.Trie.longest_prefix t "zzz"))

(* The multimap: a map from keys to sets of values, so adding the same pair
   twice is idempotent and removing the last value drops the key. *)

let test_multimap () =
  Testing.section "Multimap" (fun () ->
    Testing.check "a fresh multimap is empty" (fun () -> MM.is_empty MM.empty);
    let m = MM.add 1 10 (MM.add 1 20 (MM.add 2 30 MM.empty)) in
    Testing.check "it is no longer empty once a pair is added"
      (fun () -> not (MM.is_empty m));
    Testing.check_int "cardinal counts pairs" ~expected:3 (MM.cardinal m);
    Testing.check_int "cardinal_set counts keys" ~expected:2 (MM.cardinal_set m);
    Testing.check "a pair that was added is present" (fun () -> MM.mem 1 10 m);
    Testing.check "a pair that was not added is absent" (fun () -> not (MM.mem 1 99 m));
    Testing.check "a key that was never added is absent" (fun () -> not (MM.mem 9 10 m));
    Testing.check_int "adding the same pair twice changes nothing"
      ~expected:3 (MM.cardinal (MM.add 1 10 m));
    Testing.check_int "removing a pair drops just that pair"
      ~expected:2 (MM.cardinal (MM.remove 1 10 m));
    Testing.check "the key survives while it still has a value"
      (fun () -> MM.mem 1 20 (MM.remove 1 10 m));
    Testing.check_int "removing the last value of a key drops the key"
      ~expected:1 (MM.cardinal_set (MM.remove 2 30 m));
    Testing.check_int "the smallest value under a key"
      ~expected:10 (MM.find_min_elt 1 m);
    Testing.check_int "the largest value under a key"
      ~expected:20 (MM.find_max_elt 1 m);
    Testing.check "asking for a missing key returns None"
      (fun () -> MM.find_min_elt_opt 9 m = None))

(* Transitive closure: equivalence classes built by merging the sets they are
   declared in.  [iter] calls its argument once per class to obtain a callback
   for that class, which is what lets a caller group without collecting. *)

let test_transitive_closure () =
  Testing.section "Transitive closure" (fun () ->
    let closure_of sets =
      let tc = TC.empty () in
      List.iter (fun l -> TC.add_equivalences tc (IntSet.of_list l)) sets;
      tc in
    (* The shape of the partition, as "members/members", classes sorted so the
       comparison does not depend on the order they come out in. *)
    let partition tc =
      let classes = ref [] in
      TC.iter
        (fun () ->
          let cls = ref [] in
          List.accum classes cls;
          fun e -> List.accum cls e)
        tc;
      List.rev !classes
      |> List.map (fun cls -> List.sort compare !cls |> List.map string_of_int
                              |> String.concat ",")
      |> List.sort compare |> String.concat "/" in
    Testing.check_int "an empty closure holds nothing"
      ~expected:0 (TC.cardinal (closure_of []));
    Testing.check_string "and has no classes" ~expected:"" (partition (closure_of []));
    Testing.check_int "a declared pair contributes two elements"
      ~expected:2 (TC.cardinal (closure_of [ [ 1; 2 ] ]));
    Testing.check_string "which form one class"
      ~expected:"1,2" (partition (closure_of [ [ 1; 2 ] ]));
    (* The point of the structure: two overlapping declarations are one class. *)
    Testing.check_string "overlapping pairs merge into one class"
      ~expected:"1,2,3" (partition (closure_of [ [ 1; 2 ]; [ 2; 3 ] ]));
    Testing.check_int "and the elements are counted once each"
      ~expected:3 (TC.cardinal (closure_of [ [ 1; 2 ]; [ 2; 3 ] ]));
    Testing.check_string "disjoint pairs stay apart"
      ~expected:"1,2/4,5" (partition (closure_of [ [ 1; 2 ]; [ 4; 5 ] ]));
    (* A later declaration can join two classes that were separate. *)
    Testing.check_string "a bridging declaration merges two classes"
      ~expected:"1,2,4,5" (partition (closure_of [ [ 1; 2 ]; [ 4; 5 ]; [ 1; 4 ] ]));
    Testing.check_string "merging is not order-dependent"
      ~expected:(partition (closure_of [ [ 1; 2 ]; [ 4; 5 ]; [ 1; 4 ] ]))
      (partition (closure_of [ [ 1; 4 ]; [ 4; 5 ]; [ 1; 2 ] ]));
    Testing.check_string "declaring the same pair twice changes nothing"
      ~expected:"1,2" (partition (closure_of [ [ 1; 2 ]; [ 1; 2 ] ]));
    Testing.check_string "a singleton declaration is its own class"
      ~expected:"1,2/9" (partition (closure_of [ [ 1; 2 ]; [ 9 ] ]));
    (* A declaration of three at once is one class, not three pairs. *)
    Testing.check_string "a set of three is a single class"
      ~expected:"1,2,3" (partition (closure_of [ [ 1; 2; 3 ] ]));
    Testing.check_string "a chain closes transitively"
      ~expected:"1,2,3,4,5"
      (partition (closure_of [ [ 1; 2 ]; [ 2; 3 ]; [ 3; 4 ]; [ 4; 5 ] ])))

let run () =
  test_arraystack ();
  test_trie ();
  test_multimap ();
  test_transitive_closure ()
