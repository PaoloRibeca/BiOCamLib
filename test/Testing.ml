(*
    Testing.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Testing.ml is the small assertion harness shared by every test
    suite under test/.  It provides grouped checks, a summary that
    exits non-zero when something is wrong, and -- the reason it
    exists rather than plain [assert] -- a [~known_bug] marker that
    inverts the verdict for a check pinning a defect that has been
    diagnosed but not yet fixed.  A marked check that fails is
    reported as a known failure and does not fail the run; a marked
    check that PASSES is an error, because it means the defect was
    fixed and the marker should now be removed.  That way the suite
    stays green while still carrying an executable record of every
    outstanding bug, and tells us the moment one of them goes away.

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

include (
  struct
    (* Verdict of one check.  [Known_failure] is a marked check that failed as
       expected; [Unexpected_pass] is a marked check that passed, which counts
       against the run because the marker is now stale. *)
    module Outcome =
      struct
        type t =
          | Passed
          | Failed
          | Known_failure
          | Unexpected_pass
        let to_string = function
          | Passed -> "pass"
          | Failed -> "FAIL"
          | Known_failure -> "known"
          | Unexpected_pass -> "STALE"
        let of_string = function
          | "pass" -> Passed
          | "FAIL" -> Failed
          | "known" -> Known_failure
          | "STALE" -> Unexpected_pass
          | w -> Exception.raise_unrecognized_initializer __FUNCTION__ "outcome" w
      end
    (* Counters.  Held in refs rather than threaded through, since a suite is a
       flat sequence of side-effecting checks run once per process. *)
    let passed = ref 0
    and failed = ref 0
    and known = ref 0
    and stale = ref 0
    and current_group = ref ""
    let group name =
      current_group := name;
      Printf.eprintf "\n%s\n%!" (String.TermIO.bold name)
    (* Emit one result line.  The detail is only shown when it carries
       information -- a passing check has nothing to explain. *)
    let report outcome name detail =
      let tag =
        match outcome with
        | Outcome.Passed -> String.TermIO.green "  pass "
        | Outcome.Failed -> String.TermIO.red "  FAIL "
        | Outcome.Known_failure -> String.TermIO.grey "  known"
        | Outcome.Unexpected_pass -> String.TermIO.red "  STALE" in
      Printf.eprintf "%s %s%s\n%!" tag name
        (if detail = "" then "" else Printf.sprintf "\n         %s" (String.TermIO.grey detail))
    (* The single entry point every other check funnels through.  [f] returns
       [true, detail] when the property holds; [detail] explains the failure and
       is ignored on success.  An exception escaping [f] counts as a failure
       whose detail is the exception, so a check can never abort the run. *)
    let verify ?known_bug name f =
      let ok, detail = try f () with e -> false, Printexc.to_string e in
      match known_bug, ok with
      | None, true -> incr passed; report Outcome.Passed name ""
      | None, false -> incr failed; report Outcome.Failed name detail
      | Some why, false ->
        incr known;
        report Outcome.Known_failure name (Printf.sprintf "known bug: %s -- %s" why detail)
      | Some why, true ->
        incr stale;
        report Outcome.Unexpected_pass name
          (Printf.sprintf "marked as a known bug but PASSED, so drop the marker: %s" why)
    let check ?known_bug name f = verify ?known_bug name (fun () -> f (), "")
    (* Run a whole group.  Setup that lives between checks can raise -- a
       malformed fixture, a parser that rejects what we thought it accepted --
       and that must not abort the remaining groups, so it is caught here and
       charged to the group as one failure. *)
    let section name f =
      group name;
      try f () with e ->
        incr failed;
        report Outcome.Failed "(group setup)"
          (Printf.sprintf "raised outside any check: %s" (Printexc.to_string e))
    (* Equality checks.  Each takes the value's printer so a failure says what
       was expected and what arrived, which is the whole value of a harness over
       a bare [assert]. *)
    let check_equal ?known_bug name ~to_string ~expected got =
      verify ?known_bug name (fun () ->
        expected = got,
        Printf.sprintf "expected %s, got %s" (to_string expected) (to_string got))
    let check_string ?known_bug name ~expected got =
      check_equal ?known_bug name ~to_string:(Printf.sprintf "%S") ~expected got
    let check_int ?known_bug name ~expected got =
      check_equal ?known_bug name ~to_string:string_of_int ~expected got
    let check_bool ?known_bug name ~expected got =
      check_equal ?known_bug name ~to_string:string_of_bool ~expected got
    (* [f] must raise.  When [~re] is given the stringified exception must also
       match it, which is how we pin the *reason* a malformed input is rejected
       rather than merely that it is. *)
    let check_raises ?known_bug ?re name f =
      verify ?known_bug name (fun () ->
        match f () with
        | exception e ->
          let msg = Printexc.to_string e in
          (match re with
           | None -> true, ""
           | Some re ->
             Str.matches (Str.regexp re) msg,
             Printf.sprintf "raised %S, which does not match %S" msg re)
        | _ -> false, "returned normally, but was expected to raise")
    let check_does_not_raise ?known_bug name f =
      verify ?known_bug name (fun () ->
        match f () with
        | exception e -> false, Printf.sprintf "raised %s" (Printexc.to_string e)
        | _ -> true, "")
    let summary () =
      let total = !passed + !failed + !known + !stale in
      Printf.eprintf "\n%s\n" (String.TermIO.bold "Summary");
      Printf.eprintf "  %s\n"
        (Printf.sprintf "ran: %d, passed: %d, failed: %d, known-failing: %d, stale-markers: %d"
           total !passed !failed !known !stale);
      if !known > 0 then
        Printf.eprintf "  %s\n"
          (String.TermIO.grey
             (Printf.sprintf "%d %s a diagnosed but unfixed defect; see the design note"
                !known (String.pluralize_int ~plural:"checks pin" "check pins" !known)));
      if !stale > 0 then
        Printf.eprintf "  %s\n"
          (String.TermIO.red
             (Printf.sprintf "%d known-bug %s now passing and must be un-marked"
                !stale (String.pluralize_int ~plural:"markers are" "marker is" !stale)));
      if !failed = 0 && !stale = 0 then begin
        Printf.eprintf "  %s\n%!" (String.TermIO.green "OK");
        exit 0
      end else begin
        Printf.eprintf "  %s\n%!" (String.TermIO.red "FAILED");
        exit 1
      end
  end: sig
    module Outcome:
      sig
        type t =
          | Passed
          | Failed
          | Known_failure
          | Unexpected_pass
        val to_string: t -> string
        val of_string: string -> t
      end
    val group: string -> unit
    val section: string -> (unit -> unit) -> unit
    val check: ?known_bug:string -> string -> (unit -> bool) -> unit
    val check_equal:
      ?known_bug:string -> string -> to_string:('a -> string) -> expected:'a -> 'a -> unit
    val check_string: ?known_bug:string -> string -> expected:string -> string -> unit
    val check_int: ?known_bug:string -> string -> expected:int -> int -> unit
    val check_bool: ?known_bug:string -> string -> expected:bool -> bool -> unit
    val check_raises: ?known_bug:string -> ?re:string -> string -> (unit -> 'a) -> unit
    val check_does_not_raise: ?known_bug:string -> string -> (unit -> 'a) -> unit
    val summary: unit -> unit
  end
)
