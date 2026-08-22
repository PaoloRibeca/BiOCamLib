(*
    Tests_Better.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Tests_Better.ml exercises the ambient vocabulary every other module
    is written in: the string splitters, the pluralisation helpers that
    every diagnostic message goes through, the accumulators, and the
    rounding.  None of it is deep, and that is the point -- a change
    here moves under everything at once, so the behaviour is worth
    stating rather than assuming.

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

(* Helpers. *)

let show l = String.concat "|" l

(* Splitting.  [on_char_as_list] is what every tab-separated reader in the
   library is built on, so its treatment of empty fields is load-bearing: a
   GFF3 row with an empty column must still yield nine fields. *)

let test_split () =
  Testing.section "String splitting" (fun () ->
    Testing.check_string "a string splits on a character"
      ~expected:"a|b|c" (show (String.Split.on_char_as_list ',' "a,b,c"));
    Testing.check_string "an empty field in the middle is kept"
      ~expected:"a||c" (show (String.Split.on_char_as_list ',' "a,,c"));
    Testing.check_string "a leading empty field is kept"
      ~expected:"|b" (show (String.Split.on_char_as_list ',' ",b"));
    Testing.check_string "a trailing empty field is kept"
      ~expected:"a|" (show (String.Split.on_char_as_list ',' "a,"));
    Testing.check_string "a string with no separator yields itself"
      ~expected:"abc" (show (String.Split.on_char_as_list ',' "abc"));
    Testing.check_string "the empty string yields one empty field"
      ~expected:"" (show (String.Split.on_char_as_list ',' ""));
    Testing.check_int "splitting a tab-separated row counts every column"
      ~expected:9
      (List.length (String.Split.on_char_as_list '\t' "1\t2\t3\t4\t5\t6\t7\t8\t9"));
    Testing.check_int "and still does when a column is empty"
      ~expected:9
      (List.length (String.Split.on_char_as_list '\t' "1\t2\t3\t4\t\t6\t7\t8\t9"));
    Testing.check_string "the array form agrees with the list form"
      ~expected:"a||c"
      (show (Array.to_list (String.Split.on_char_as_array ',' "a,,c"))))

(* Pluralisation.  Every count printed by a tool in this family goes through
   these, so an off-by-one in the boundary shows up everywhere at once. *)

let test_pluralize () =
  Testing.section "Pluralisation" (fun () ->
    Testing.check_string "one is singular" ~expected:"feature"
      (String.pluralize_int "feature" 1);
    Testing.check_string "two is plural" ~expected:"features"
      (String.pluralize_int "feature" 2);
    Testing.check_string "zero is plural" ~expected:"features"
      (String.pluralize_int "feature" 0);
    (* A negative count is still not one, so it takes the plural. *)
    Testing.check_string "a negative count is plural" ~expected:"features"
      (String.pluralize_int "feature" (-1));
    (* An irregular plural has to be given, since the default just adds an s. *)
    Testing.check_string "an irregular plural is used when supplied"
      ~expected:"complexes" (String.pluralize_int ~plural:"complexes" "complex" 3);
    Testing.check_string "and the singular still wins at one"
      ~expected:"complex" (String.pluralize_int ~plural:"complexes" "complex" 1);
    Testing.check_string "the float form pluralises at one too"
      ~expected:"base" (String.pluralize_float "base" 1.);
    Testing.check_string "and a fractional count is plural"
      ~expected:"bases" (String.pluralize_float "base" 1.5))

(* Accumulators.  [List.accum] prepends, which is why every caller reverses at
   the end; stating it here stops the reversal looking gratuitous. *)

let test_accum () =
  Testing.section "Accumulators" (fun () ->
    Testing.check_string "List.accum prepends"
      ~expected:"3|2|1"
      (let l = ref [] in
       List.accum l "1"; List.accum l "2"; List.accum l "3";
       show !l);
    Testing.check_string "so the caller reverses to recover the order"
      ~expected:"1|2|3"
      (let l = ref [] in
       List.accum l "1"; List.accum l "2"; List.accum l "3";
       show (List.rev !l));
    Testing.check_int "List.pop takes the most recent element"
      ~expected:3 (let l = ref [ 3; 2; 1 ] in List.pop l);
    Testing.check_int "and shortens the list"
      ~expected:2 (let l = ref [ 3; 2; 1 ] in ignore (List.pop l); List.length !l);
    Testing.check "List.pop_opt returns None when there is nothing left"
      (fun () -> List.pop_opt (ref []) = None);
    Testing.check_raises "List.pop raises when there is nothing left"
      (fun () -> ignore (List.pop (ref [])));
    Testing.check_string "String.accum appends rather than prepending"
      ~expected:"abc"
      (let s = ref "" in
       String.accum s "a"; String.accum s "b"; String.accum s "c";
       !s))

(* Miscellany: reversal, rounding, and the terminal helpers that every verbose
   line is wrapped in. *)

let test_misc () =
  Testing.section "String and float helpers" (fun () ->
    Testing.check_string "a string reverses" ~expected:"cba" (String.rev "abc");
    Testing.check_string "reversing twice is the identity"
      ~expected:"abcde" (String.rev (String.rev "abcde"));
    Testing.check_string "the empty string reverses to itself"
      ~expected:"" (String.rev "");
    (* compare_lexicolength orders by length first, then lexically, which is
       what makes a sorted option list read sensibly. *)
    Testing.check "a shorter string sorts before a longer one"
      (fun () -> String.compare_lexicolength "ab" "abc" < 0);
    Testing.check "equal lengths fall back to the lexical order"
      (fun () -> String.compare_lexicolength "abc" "abd" < 0);
    Testing.check "a string compares equal to itself"
      (fun () -> String.compare_lexicolength "abc" "abc" = 0);
    Testing.check_float "rounding goes to the nearest integer"
      ~expected:3. (Float.round 2.6);
    Testing.check_float "a half rounds away from zero"
      ~expected:3. (Float.round 2.5);
    Testing.check_float "and so does a negative half"
      ~expected:(-3.) (Float.round (-2.5));
    (* The terminal helpers have to leave the text itself intact, since a
       message is often matched on by the test suite of a companion tool. *)
    Testing.check "bold contains the string it decorates"
      (fun () ->
        let s = String.TermIO.bold "hello" in
        String.length s > 5
          && (try ignore (Str.search_forward (Str.regexp_string "hello") s 0); true
              with Not_found -> false)))

let run () =
  test_split ();
  test_pluralize ();
  test_accum ();
  test_misc ()
