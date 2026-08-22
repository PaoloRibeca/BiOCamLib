(*
    Tests_Files.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Tests_Files.ml exercises the marshalling of a quoted path.  A
    QuotedPath.t carries a name twice, once as it is and once as a
    shell would need it written, and the pair is marshalled into a
    single tab-separated string.  Both halves are escaped and the
    result escaped again, which looks redundant until the name itself
    contains a tab: without the second pass the separator would be
    indistinguishable from the content and the pair would come back
    split in the wrong place.  Those are the cases checked here.

    The parts of Files that read sequence data are not covered: they
    take channels and file names, and what they do with them is a
    separate concern from what this file is about.

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

module Q = Files.QuotedPath

(* Helpers. *)

let qp unquoted quoted = { Q.unquoted; quoted }

let show (p: Q.t) = Printf.sprintf "%S|%S" p.Q.unquoted p.Q.quoted

(* A path survives marshalling iff both halves come back exactly. *)
let round_trips p = show (Q.of_string (Q.to_string p)) = show p

let test_quoted_path () =
  Testing.section "Quoted paths" (fun () ->
    Testing.check_string "the empty path has two empty halves"
      ~expected:"\"\"|\"\"" (show Q.none);
    Testing.check "the empty path survives marshalling" (fun () -> round_trips Q.none);
    Testing.check "an ordinary path survives"
      (fun () -> round_trips (qp "/tmp/file.txt" "/tmp/file.txt"));
    (* The quoted half is what a shell would need, so it routinely differs from
       the unquoted one; both have to come back. *)
    Testing.check "a path whose halves differ survives"
      (fun () -> round_trips (qp "/tmp/a b" "/tmp/a\\ b"));
    Testing.check_string "and the halves are not transposed"
      ~expected:"\"/tmp/a b\"|\"/tmp/a\\\\ b\""
      (show (Q.of_string (Q.to_string (qp "/tmp/a b" "/tmp/a\\ b"))));
    (* The case the double escaping exists for. *)
    Testing.check "a path containing a tab survives"
      (fun () -> round_trips (qp "/tmp/a\tb" "/tmp/a\\\tb"));
    Testing.check_string "and the tab is still a tab afterwards"
      ~expected:"/tmp/a\tb"
      ((Q.of_string (Q.to_string (qp "/tmp/a\tb" "x"))).Q.unquoted);
    (* Backslashes and newlines are the other two that escaping has to survive
       without either doubling up or collapsing. *)
    Testing.check "a path containing a backslash survives"
      (fun () -> round_trips (qp "/tmp/a\\b" "/tmp/a\\\\b"));
    Testing.check "a path containing a newline survives"
      (fun () -> round_trips (qp "/tmp/a\nb" "/tmp/a\\nb"));
    Testing.check "a path containing a double quote survives"
      (fun () -> round_trips (qp "/tmp/a\"b" "/tmp/a\\\"b"));
    Testing.check "a path containing a single quote survives"
      (fun () -> round_trips (qp "/tmp/a'b" "'/tmp/a'\\''b'"));
    (* Several at once, since escaping bugs tend to show up on interaction. *)
    Testing.check "a path containing a tab and a backslash survives"
      (fun () -> round_trips (qp "a\t\\b" "c\\\td"));
    Testing.check "a path that is nothing but separators survives"
      (fun () -> round_trips (qp "\t\t" "\\t\\t"));
    (* A string that never came from to_string is not a marshalled pair. *)
    Testing.check_raises "a string with no separator is refused"
      (fun () -> ignore (Q.of_string "no-separator-here"));
    Testing.check_raises "a string with too many separators is refused"
      (fun () -> ignore (Q.of_string "a\\tb\\tc")))

let run () =
  test_quoted_path ()
