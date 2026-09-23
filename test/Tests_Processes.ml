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
      (Processes.Parallel.get_nproc () >= 1);
    (* The count is read off the machine, so no test can name it.  What can be said
       is that where nproc exists it is the answer -- which is what the fallback
       added beside it must not disturb.  Where it does not exist, on a Mac, this
       check holds vacuously, that branch being unreachable from here *)
    Testing.check_bool "and is nproc's own answer wherever nproc exists" ~expected:true
      (if Sys.command "command -v nproc > /dev/null 2>&1" <> 0 then
        true
      else
        Processes.Parallel.get_nproc () = int_of_string (S.spawn_and_read_single_line "nproc")))

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

(* A caller may end a section from its output function by raising Stop.  What
   it has been handed is then exactly the results up to the one that stopped
   it, in order, and the section returns at once even with a worker still busy
   on a later item: the processes it forked are killed rather than waited for,
   which is the whole point of stopping. *)

let test_process_stream_chunkwise_stop () =
  Testing.section "Parallel streams stopped early" (fun () ->
    let stopped_at ~slow threads =
      let next = ref 0 and acc = ref [] in
      Processes.Parallel.process_stream_chunkwise
        (fun () -> if !next >= 200 then raise End_of_file else (incr next; !next))
        (fun x -> if x = slow then Unix.sleepf 60.; x * x)
        (fun y -> List.accum acc y; if y = 100 then raise Processes.Parallel.Stop)
        threads;
      List.rev !acc in
    let show l = List.map string_of_int l |> String.concat "," in
    let expected = show (List.init 10 (fun i -> (i + 1) * (i + 1))) in
    Testing.check_string "the results up to the stop come back, in order, and no more"
      ~expected (show (stopped_at ~slow:0 4));
    Testing.check_string "and so they do on one thread" ~expected (show (stopped_at ~slow:0 1));
    let started = Unix.gettimeofday () in
    let got = stopped_at ~slow:12 4 in
    Testing.check_bool "a worker busy past the stop is killed, not waited for" ~expected:true
      (Unix.gettimeofday () -. started < 30.);
    Testing.check_string "and what came back is the same" ~expected (show got))

(* Stopping, looked at harder, because this function runs nearly every tool
   built on the library.  Wherever the stop falls, on any number of threads,
   what comes back is the prefix up to it, in order.  Every process the
   section forked is gone by the time it returns, whether it had finished its
   work or was killed in the middle of it.  A caller that holds SIGTERM back
   itself -- the signal the section uses to stop -- can still stop a section,
   and keeps its own mask.  And many sections in a row, stopped or not, leave
   nothing behind. *)

let test_process_stream_chunkwise_stop_hard () =
  Testing.section "Parallel streams stopped early, harder" (fun () ->
    let pids_file = Filename.temp_file "BiOCamLib_Tests_" ".pids" in
    Fun.protect ~finally:(fun () -> Sys.remove pids_file) (fun () ->
      (* Each item notes the pid of the worker running it, in one write that a kill cannot
         cut in half, so that every worker can be checked to be gone afterwards *)
      let note_pid () =
        let fd = Unix.openfile pids_file [ Unix.O_WRONLY; Unix.O_APPEND; Unix.O_CREAT ] 0o644 in
        let line = Printf.sprintf "%d\n" (Unix.getpid ()) in
        ignore (Unix.write_substring fd line 0 (String.length line));
        Unix.close fd in
      let workers_gone () =
        let ic = open_in pids_file in
        let rec read acc =
          match input_line ic with
          | line -> read (int_of_string line :: acc)
          | exception End_of_file -> acc in
        let pids = read [] in
        close_in ic;
        close_out (open_out pids_file);
        pids <> []
          && List.for_all
              (fun pid ->
                match Unix.kill pid 0 with
                | () -> false
                | exception Unix.Unix_error (Unix.ESRCH, _, _) -> true)
              pids in
      let run ~items ~stop_at ~slow threads =
        let next = ref 0 and acc = ref [] in
        Processes.Parallel.process_stream_chunkwise
          (fun () -> if !next >= items then raise End_of_file else (incr next; !next))
          (fun x -> note_pid (); if x = slow then Unix.sleepf 60.; x)
          (fun y -> List.accum acc y; if y = stop_at then raise Processes.Parallel.Stop)
          threads;
        List.rev !acc in
      let prefix k = List.init k (fun i -> i + 1) in
      let failures = ref [] in
      let expect what got expected =
        if got <> expected then List.accum failures what in
      List.iter
        (fun threads ->
          List.iter
            (fun stop_at ->
              let what = Printf.sprintf "stop at %d on %d threads" stop_at threads in
              let got = run ~items:30 ~stop_at ~slow:0 threads in
              expect what got (prefix (min stop_at 30));
              if not (workers_gone ()) then
                List.accum failures (what ^ " left a worker"))
            [ 1; 2; 7; 29; 30; 31 ])
        [ 1; 2; 4; 16 ];
      Testing.check_string
        "every stop point, and none, on 1, 2, 4 and 16 threads: the prefix, in order, and no worker left"
        ~expected:"" (List.rev !failures |> String.concat "; ");
      let started = Unix.gettimeofday () in
      let got = run ~items:30 ~stop_at:3 ~slow:5 4 in
      Testing.check_bool "a worker killed in the middle of an item is gone as well" ~expected:true
        (got = prefix 3 && Unix.gettimeofday () -. started < 30. && workers_gone ());
      let caller_mask = Unix.sigprocmask Unix.SIG_BLOCK [ Sys.sigterm ] in
      let started = Unix.gettimeofday () in
      let got = run ~items:30 ~stop_at:3 ~slow:5 4 in
      let elapsed = Unix.gettimeofday () -. started in
      let still_blocked = List.mem Sys.sigterm (Unix.sigprocmask Unix.SIG_BLOCK []) in
      ignore (Unix.sigprocmask Unix.SIG_SETMASK caller_mask);
      Testing.check_bool "a caller holding SIGTERM back can still stop, and keeps its mask"
        ~expected:true (got = prefix 3 && elapsed < 30. && still_blocked && workers_gone ());
      failures := [];
      for i = 1 to 100 do
        let stop_at = if i mod 2 = 0 then 1 + i mod 20 else 21 in
        expect (Printf.sprintf "section %d" i) (run ~items:20 ~stop_at ~slow:0 4)
          (prefix (min stop_at 20))
      done;
      Testing.check_string "a hundred sections in a row, stopped or not, are each exact"
        ~expected:"" (List.rev !failures |> String.concat "; ");
      Testing.check_bool "and leave no worker behind" ~expected:true (workers_gone ())))

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
  test_process_stream_chunkwise_stop ();
  test_process_stream_chunkwise_stop_hard ();
  test_process_stream_linewise ()
