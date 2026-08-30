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
      (partition (closure_of [ [ 1; 2 ]; [ 2; 3 ]; [ 3; 4 ]; [ 4; 5 ] ]));
    (* THE MERGE HOOK.  It exists so that relations fed in increasing distance order
       emit a single-linkage dendrogram bottom-up -- one node per firing -- without the
       caller having to snapshot the whole partition after every relation, which is what
       makes this structure usable for clustering and not only for closure. *)
    let merges_of sets =
      let log = ref [] in
      let tc = TC.empty ~on_merge:(fun a b -> List.accum log (a, b)) () in
      List.iter (fun l -> TC.add_equivalences tc (IntSet.of_list l)) sets;
      List.rev !log in
    let merges_to_string l =
      List.map (fun (a, b) -> Printf.sprintf "%d+%d" a b) l |> String.concat " " in
    (* A PAIR OF FRESH ELEMENTS IS ITSELF A MERGE -- of two singleton classes -- and
       it is the commonest one, since a distance matrix is fed as pairs.  The first
       version of this hook fired only on absorption and so emitted nothing here,
       leaving a dendrogram short of a node for every leaf pairing. *)
    Testing.check_string "a pair of fresh elements is one merge"
      ~expected:"1+2" (merges_of [ [ 1; 2 ] ] |> merges_to_string);
    Testing.check_string "and a fresh triple is two"
      ~expected:"1+2 1+3" (merges_of [ [ 1; 2; 3 ] ] |> merges_to_string);
    Testing.check_string "joining two standing classes fires once more"
      ~expected:"1+2 3+4 1+3" (merges_of [ [ 1; 2 ]; [ 3; 4 ]; [ 2; 3 ] ] |> merges_to_string);
    (* One firing per class joined, not one per call: a relation spanning three
       classes performs two merges and must say so, or the log is not a tree. *)
    Testing.check_string "a relation spanning three classes fires twice"
      ~expected:"1+2 3+4 5+6 1+3 1+5"
      (merges_of [ [ 1; 2 ]; [ 3; 4 ]; [ 5; 6 ]; [ 1; 3; 5 ] ] |> merges_to_string);
    Testing.check_string "a relation inside one class fires nothing"
      ~expected:"1+2 3+4 1+3"
      (merges_of [ [ 1; 2 ]; [ 3; 4 ]; [ 2; 3 ]; [ 1; 4 ] ] |> merges_to_string);
    (* THE DENDROGRAM PROPERTY, which is the whole point: n elements ending in one
       class take exactly n-1 merges, whatever order the relations arrive in.  It is
       what tells you the log is a tree over the leaves and not a list of events. *)
    Testing.check_int "a chain of five emits four merges"
      ~expected:4
      (List.length (merges_of [ [ 1; 2 ]; [ 2; 3 ]; [ 3; 4 ]; [ 4; 5 ] ]));
    Testing.check_int "and so does the same chain shuffled"
      ~expected:4
      (List.length (merges_of [ [ 3; 4 ]; [ 1; 2 ]; [ 4; 5 ]; [ 2; 3 ] ]));
    (* WITH AND WITHOUT THE HOOK THE ANSWER IS THE SAME.  It observes; it must not
       participate -- and it reads the state BEFORE each absorption, which is the one
       place a representative could be read off a half-merged structure. *)
    Testing.check_string "the hook does not change the partition"
      ~expected:(partition (closure_of [ [ 1; 2 ]; [ 3; 4 ]; [ 1; 3 ]; [ 9 ] ]))
      (let tc = TC.empty ~on_merge:(fun _ _ -> ()) () in
       List.iter (fun l -> TC.add_equivalences tc (IntSet.of_list l))
         [ [ 1; 2 ]; [ 3; 4 ]; [ 1; 3 ]; [ 9 ] ];
       partition tc))

(* Command-line scaffolding.  [parse] itself and the [get_parameter_*] family
   cannot be driven from here, and the reason is not that they exit: there is
   exactly one [exit] in the module, in the error path, and exiting is the right
   thing for a parser whose only callers are drivers.  It is that [_argv] is
   bound to [Sys.argv] once, at module level, so a check has no way to hand the
   parser a command line of its own.  Everything that does not read the command
   line is fair game, and that is the header and synopsis formatting and the
   separator constructors. *)

module TA = Tools.Argv

let captured f =
  let path = Filename.temp_file "BiOCamLib_Tests_" ".txt" in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
    let oc = open_out path in
    f oc;
    close_out oc;
    let ic = open_in path in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    s)

(* The usage is colourised, so a value printed inside quotes has escape
   sequences sitting between it and them.  Checking what the terminal SAYS,
   rather than how it paints it, means taking those out first. *)
let uncoloured = Str.global_replace (Str.regexp "\027\\[[0-9;]*m") ""

let contains needle haystack =
  let n = String.length needle and l = String.length haystack in
  let rec walk i = i + n <= l && (String.sub haystack i n = needle || walk (i + 1)) in
  n = 0 || walk 0

let test_argv () =
  Testing.section "Command-line scaffolding" (fun () ->
    (* A separator is an option with no names and no action: it exists only to
       put a line of text into the usage between groups of real options. *)
    let names, arg, help, kind, action = TA.make_separator "Group:" in
    Testing.check_int "a separator names no option" ~expected:0 (List.length names);
    Testing.check_bool "and takes no argument" ~expected:true (arg = None);
    Testing.check_string "its help is the text it was given"
      ~expected:"Group:" (String.concat "" help);
    Testing.check_bool "it is optional, so nothing demands it" ~expected:true
      (kind = TA.Optional);
    Testing.check_does_not_raise "and its action does nothing"
      (fun () -> action "");
    let _, _, help, _, _ = TA.make_separator_multiline [ "one"; "two" ] in
    Testing.check_string "a multi-line separator keeps every line"
      ~expected:"one|two" (String.concat "|" help);
    (* The synopsis is stored and printed back verbatim. *)
    TA.set_synopsis "<input> [OPTIONS]";
    Testing.check_string "the synopsis is printed as it was set"
      ~expected:"<input> [OPTIONS]" (captured (fun oc -> TA.synopsis ~output:oc ()));
    (* The header is drawn as a box around the program's identity, so what is
       worth pinning is that the identity and each dependency reach it -- not
       the box, which is decoration and would make the check a transcript. *)
    TA.set_header
      ({ TA.name = "TestTool"; version = "9"; date = "01-Jan-2026" },
       [ "2026", "A Name", "a@example.com" ],
       [ { TA.name = "BiOCamLib"; version = "1"; date = "02-Jan-2026" } ]);
    let printed = captured (fun oc -> TA.header ~output:oc ()) in
    List.iter (fun needle ->
      Testing.check_bool (Printf.sprintf "the header carries %S" needle)
        ~expected:true (contains needle printed))
      [ "TestTool"; "9"; "01-Jan-2026"; "BiOCamLib"; "02-Jan-2026" ];
    (* Setting it again replaces rather than accumulates, which matters because
       a driver calls this once and a test calling it twice must not see both. *)
    TA.set_header
      ({ TA.name = "Other"; version = "1"; date = "03-Jan-2026" }, [], []);
    let printed = captured (fun oc -> TA.header ~output:oc ()) in
    Testing.check_bool "setting the header again replaces it" ~expected:true
      (contains "Other" printed && not (contains "TestTool" printed)))


(* Parsing, now that a command line can be handed in.  Only the accepting paths
   are reachable: every rejection -- an unknown option, a missing mandatory one,
   a parameter of the wrong shape -- ends in the module's single [exit], which
   is right for a driver and fatal for a suite. *)

let test_argv_parse () =
  Testing.section "Command-line parsing" (fun () ->
    TA.set_header ({ TA.name = "T"; version = "1"; date = "01-Jan-2026" }, [], []);
    TA.set_synopsis "[OPTIONS]";
    let fired = ref [] in
    let note s = List.accum fired s in
    (* An action is handed the option it was reached by and pulls its own
       parameters off the command line as it goes. *)
    TA.parse ~argv:[| "prog"; "-n"; "5"; "--name"; "abc"; "-f"; "2.5" |]
      [ [ "-n"; "--number" ], Some "<n>", [ "a number" ], TA.Optional,
          (fun _ -> Printf.sprintf "n=%d" (TA.get_parameter_int ()) |> note);
        [ "--name" ], Some "<s>", [ "a name" ], TA.Optional,
          (fun _ -> Printf.sprintf "name=%s" (TA.get_parameter ()) |> note);
        [ "-f" ], Some "<x>", [ "a float" ], TA.Optional,
          (fun _ -> Printf.sprintf "f=%g" (TA.get_parameter_float ()) |> note) ];
    Testing.check_string "each option's action fires, with its own parameter"
      ~expected:"n=5 name=abc f=2.5" (List.rev !fired |> String.concat " ");
    (* The long spelling of an option reaches the same action as the short one. *)
    fired := [];
    TA.parse ~argv:[| "prog"; "--number"; "7" |]
      [ [ "-n"; "--number" ], Some "<n>", [ "a number" ], TA.Optional,
          (fun _ -> Printf.sprintf "n=%d" (TA.get_parameter_int ()) |> note) ];
    Testing.check_string "an option is reached by either of its names"
      ~expected:"n=7" (List.rev !fired |> String.concat " ");
    (* Parsing a second command line starts afresh: the cursor is reset, so the
       two parses above did not run into one another. *)
    fired := [];
    TA.parse ~argv:[| "prog"; "-n"; "1" |]
      [ [ "-n" ], Some "<n>", [ "a number" ], TA.Optional,
          (fun _ -> Printf.sprintf "n=%d" (TA.get_parameter_int ()) |> note) ];
    TA.parse ~argv:[| "prog"; "-n"; "2" |]
      [ [ "-n" ], Some "<n>", [ "a number" ], TA.Optional,
          (fun _ -> Printf.sprintf "n=%d" (TA.get_parameter_int ()) |> note) ];
    Testing.check_string "each parse starts from the beginning of its own line"
      ~expected:"n=1 n=2" (List.rev !fired |> String.concat " ");
    (* Everything after the option that asks for it. *)
    let rest = ref [||] in
    TA.parse ~argv:[| "prog"; "--rest"; "a"; "b"; "c" |]
      [ [ "--rest" ], None, [ "the rest" ], TA.Optional,
          (fun _ -> rest := TA.get_remaining_parameters ()) ];
    Testing.check_string "the remaining parameters are taken as they stand"
      ~expected:"a,b,c" (Array.to_list !rest |> String.concat ",");
    (* A mandatory option is satisfied by being present. *)
    fired := [];
    Testing.check_does_not_raise "a mandatory option that is supplied parses"
      (fun () ->
        TA.parse ~argv:[| "prog"; "-m"; "x" |]
          [ [ "-m" ], Some "<s>", [ "needed" ], TA.Mandatory,
              (fun _ -> TA.get_parameter () |> note) ]);
    Testing.check_string "and its action ran" ~expected:"x"
      (List.rev !fired |> String.concat " ");
    (* [parse] is what fills the usage and its markdown twin. *)
    TA.parse ~argv:[| "prog" |]
      [ [ "-n"; "--number" ], Some "<n>", [ "how many things" ], TA.Optional, (fun _ -> ());
        [ "-w" ], Some "<keep|drop>", [ "pass 'keep' or see <https://example.org/x>" ],
          TA.Optional, (fun _ -> ());
        [ "-e" ], None,
          [ "a well-known thing -- an aside, as these are written -- and a hyphenated";
            "word, and --help named in prose" ],
          TA.Optional, (fun _ -> ());
        [ "-d" ], None, [ "a default" ], TA.Default (fun () -> "42"), (fun _ -> ());
        [ "-j" ], None, [ "detected" ], TA.Detected ((fun () -> "8"), "nproc"), (fun _ -> ());
        TA.make_separator_multiline
          [ "Heading."; "First paragraph."; ""; "Second paragraph."; "  an example" ];
        TA.make_separator_multiline [ ""; "Opens with a blank rather than a heading." ] ];
    let printed = captured (fun oc -> TA.usage ~output:oc ()) in
    List.iter (fun needle ->
      Testing.check_bool (Printf.sprintf "the usage mentions %S" needle) ~expected:true
        (contains needle printed))
      [ "-n"; "--number"; "<n>"; "how many things"; "42" ];
    (* A detected default is the one place the terminal and the page must NOT
       agree: the value is true on the machine printing it and false in a page
       generated once and read everywhere, so the terminal takes the value and
       the markdown takes the name of what was read. *)
    Testing.check_bool "the usage gives a detected default as the value read" ~expected:true
      (contains "default='8'" (uncoloured printed));
    Testing.check_bool "and never as the name of what read it" ~expected:false
      (contains "nproc" printed);
    (* The markdown twin is what a README is generated from, so it renders the
       same specs as a table.  What is worth pinning is how the three things a
       help string writes for a terminal are re-said for a page: a name in angle
       brackets, which would be eaten as an HTML tag if it reached the page as
       it stands, becomes italics; a quoted literal and a bracketed list of
       values, both of which the user types verbatim, become code; and a
       bracketed URL, which is markdown's own autolink, becomes a live link
       rather than the inert text escaping would leave. *)
    let md = captured (fun oc -> TA.markdown ~output:oc ()) in
    List.iter (fun (what, needle) ->
      Testing.check_bool (Printf.sprintf "the markdown %s" what) ~expected:true
        (contains needle md))
      [ "fences the header", "```\nThis is T version 1 [01-Jan-2026]\n```";
        "fences the invocation", "prog [OPTIONS]";
        "sets both spellings as code", "`-n`<br>`--number`";
        "sets an argument's name in italics, without its brackets", "| _n_ |";
        "sets a bracketed list of values as code, one span each", "`keep`&#124;`drop`";
        "sets a quoted literal as code", "pass `keep` or see";
        "makes a bracketed URL a live link",
          "[https://example.org/x](https://example.org/x)";
        "shows a default where there is one", "default=<mark>_42_</mark>";
        "gives a detected default as what was read, not as this machine's answer",
          "default=<mark>_nproc_</mark>";
        (* A pair of hyphens set off by spaces is how these help strings write an
           em-dash, and two escaped hyphens are two hyphens. *)
        "renders a spaced pair of hyphens as an em-dash", "thing &mdash; an aside";
        "and the second of the pair too", "written &mdash; and a";
        (* A hyphen is a list marker where a line begins and ordinary text
           everywhere else, so escaping every one cost legibility and bought
           nothing. *)
        "leaves a hyphen inside a word alone", "a hyphenated word";
        "and an option named in prose alone", "and --help named in prose" ];
    Testing.check_bool "so that no machine-dependent value is written into the page"
      ~expected:false (contains "default=<mark>_8_</mark>" md);
    (* A multiline separator is prose and examples, and markdown needs telling
       what a terminal shows by itself: a blank line is the only thing that
       separates two paragraphs, and an indented run is an example whose
       alignment a paragraph would collapse. *)
    (* The periods come back escaped, prose being escaped outside a code span. *)
    Testing.check_bool "a separator's first line is its heading" ~expected:true
      (contains "**Heading\\.**" md);
    Testing.check_bool "a blank line between two paragraphs is kept as one"
      ~expected:true (contains "First paragraph\\.\n\nSecond paragraph\\." md);
    Testing.check_bool "and an indented line becomes a fenced block, undented"
      ~expected:true (contains "```\nan example\n```" md);
    (* A block may open with a blank instead of a heading, which is how several
       of these set a paragraph off from the table above.  Emphasising nothing
       wrote an empty bold where the author had asked for nothing at all. *)
    Testing.check_bool "a separator opening with a blank emits no empty bold"
      ~expected:false (contains "****" md);
    Testing.check_bool "and its text is still there, as prose" ~expected:true
      (contains "\nOpens with a blank rather than a heading\\." md);
    Testing.check_bool "so that no raw angle bracket reaches the page" ~expected:true
      (not (contains "<n>" md)))


let run () =
  test_arraystack ();
  test_trie ();
  test_multimap ();
  test_transitive_closure ();
  test_argv ();
  test_argv_parse ()
