(*
    Tests_Processes.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Tests_Processes.ml exercises the subprocess and parallel-stream
    machinery.  Every entry point here spawns something, which is why
    it went untested for so long; the answer is to spawn only what is
    guaranteed to exist and to behave the same everywhere -- [true],
    [false] and [echo] -- and to let the stream combinators do their
    forking over inputs small enough to state the whole expected
    output.

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

module S = Processes.Subprocess

(* Classifying how a subprocess ended.  This is exposed precisely so that a
   process spawned elsewhere is judged by the same rule, so it is worth
   checking each verdict rather than only the happy one. *)

let test_termination_status () =
  Testing.section "Subprocess termination" (fun () ->
    Testing.check_does_not_raise "a clean exit is not a failure"
      (fun () -> S.handle_termination_status ~kind:(Exception.Kind.Subprocess "cmd") "cmd" ""
                   (Unix.WEXITED 0));
    Testing.check_raises ~re:"failed" "a non-zero exit is"
      (fun () -> S.handle_termination_status ~kind:(Exception.Kind.Subprocess "cmd") "cmd" ""
                   (Unix.WEXITED 1));
    Testing.check_raises ~re:"my-command" "and the message names the command"
      (fun () ->
        S.handle_termination_status ~kind:(Exception.Kind.Subprocess "my-command")
          "my-command" "" (Unix.WEXITED 1));
    Testing.check_raises ~re:"failed" "a signalled process is a failure"
      (fun () -> S.handle_termination_status ~kind:(Exception.Kind.Subprocess "cmd") "cmd" ""
                   (Unix.WSIGNALED Sys.sigkill));
    Testing.check_raises ~re:"failed" "and so is a stopped one"
      (fun () -> S.handle_termination_status ~kind:(Exception.Kind.Subprocess "cmd") "cmd" ""
                   (Unix.WSTOPPED Sys.sigstop)))

(* Spawning.  [true] and [false] exist on every system this runs on and do
   exactly one thing each, which is what makes them the right probes. *)

let test_spawn () =
  Testing.section "Spawning" (fun () ->
    Testing.check_does_not_raise "a command that succeeds returns quietly"
      (fun () -> S.spawn "true");
    Testing.check_raises ~re:"failed" "a command that fails raises"
      (fun () -> S.spawn "false");
    Testing.check_string "a single line of output is read back"
      ~expected:"hello" (S.spawn_and_read_single_line "echo hello");
    Testing.check_string "and leading and trailing space is the command's business"
      ~expected:"a b c" (S.spawn_and_read_single_line "echo a b c");
    Testing.check_bool "the number of processors is at least one" ~expected:true
      (Processes.Parallel.get_nproc () >= 1))

(* Memory accounting.  Nothing here can assert a number, but each of these has
   a range it cannot leave without something being wrong. *)

let test_memory () =
  Testing.section "Memory accounting" (fun () ->
    Testing.check_bool "the resident size is positive and finite" ~expected:true
      (let s = Processes.Memory.get_rs_size () in s > 0. && Float.is_finite s);
    Testing.check_bool "the heap size is positive and finite" ~expected:true
      (let s = Processes.Memory.get_gc_size () in s > 0. && Float.is_finite s))

(* The parallel stream combinators, which are what every filter in bin/ is
   built on.  The property that matters is not merely that every item is
   processed but that the output comes back in the order the input went in:
   a FASTA filter that silently reordered its records would still pass any
   check that only counted them. *)

let test_process_stream_chunkwise () =
  Testing.section "Parallel streams" (fun () ->
    let squares_with threads =
      let next = ref 0 and acc = ref [] in
      Processes.Parallel.process_stream_chunkwise
        (fun () -> if !next >= 20 then raise End_of_file else (incr next; !next))
        (fun x -> x * x)
        (fun y -> List.accum acc y)
        threads;
      List.rev !acc in
    let expected = List.init 20 (fun i -> (i + 1) * (i + 1)) in
    let show l = List.map string_of_int l |> String.concat "," in
    Testing.check_string "one thread returns every item, in order"
      ~expected:(show expected) (show (squares_with 1));
    Testing.check_string "and so do four"
      ~expected:(show expected) (show (squares_with 4));
    Testing.check_raises "a non-positive number of threads is refused"
      (fun () ->
        Processes.Parallel.process_stream_chunkwise
          (fun () -> raise End_of_file) (fun x -> x) (fun _ -> ()) 0))

(* The line-wise wrapper over the same machinery, which takes channels rather
   than closures and is what a filter reading stdin actually calls. *)

let test_process_stream_linewise () =
  Testing.section "Parallel line filter" (fun () ->
    let input = String.concat "\n" (List.init 50 (fun i -> Printf.sprintf "line%d" i)) ^ "\n" in
    let in_path = Filename.temp_file "BiOCamLib_Tests_" ".in"
    and out_path = Filename.temp_file "BiOCamLib_Tests_" ".out" in
    Fun.protect
      ~finally:(fun () -> Sys.remove in_path; Sys.remove out_path)
      (fun () ->
        let oc = open_out in_path in
        output_string oc input;
        close_out oc;
        let ic = open_in in_path and oc = open_out out_path in
        Processes.Parallel.process_stream_linewise ~verbose:false ic
          (fun buf _ line -> Printf.bprintf buf "%s\n" (String.uppercase_ascii line))
          oc 4;
        close_in ic;
        close_out oc;
        let ic = open_in out_path in
        let n = in_channel_length ic in
        let got = really_input_string ic n in
        close_in ic;
        Testing.check_string "every line comes back, upper-cased and in order"
          ~expected:(String.uppercase_ascii input) got))

let run () =
  test_termination_status ();
  test_spawn ();
  test_memory ();
  test_process_stream_chunkwise ();
  test_process_stream_linewise ()
