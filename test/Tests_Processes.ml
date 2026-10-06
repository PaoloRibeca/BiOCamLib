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

(* A worker whose item raises ends the section, which then raises in the
   caller -- rather than the exception carrying the worker out of the section
   and into the caller's code, and the caller seeing only a pipe that closed.
   A worker still busy elsewhere is killed, not waited for, as when stopping. *)

let test_process_stream_chunkwise_failure () =
  Testing.section "Parallel streams whose worker fails" (fun () ->
    let failing ~slow threads =
      let next = ref 0 in
      Processes.Parallel.process_stream_chunkwise
        (fun () -> if !next >= 200 then raise End_of_file else (incr next; !next))
        (fun x ->
          if x = slow then Unix.sleepf 60.;
          if x = 50 then failwith "item 50";
          x * x)
        (fun _ -> ())
        threads in
    Testing.check_raises "a failing item makes the section raise, on one thread"
      (fun () -> failing ~slow:0 1);
    Testing.check_raises "and on four" (fun () -> failing ~slow:0 4);
    let started = Unix.gettimeofday () in
    Testing.check_raises "and with a worker busy elsewhere" (fun () -> failing ~slow:60 4);
    Testing.check_bool "which is killed, not waited for" ~expected:true
      (Unix.gettimeofday () -. started < 30.);
    (* Once a worker has failed the input process kills them all, and the caller, still asking
       the others for their results, writes to workers that have gone: that must end the section
       with an exception, and not end the program with SIGPIPE.  A slow output function keeps the
       caller busy while the workers are killed, which is when it happens.  It is run in a child
       process, so that a program killed is a check failed and not the suite gone, and the child
       says nothing on stderr *)
    let survives () =
      flush_all ();
      match Unix.fork () with
      | 0 ->
        Unix.dup2 (Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0) Unix.stderr;
        for _ = 1 to 10 do
          let next = ref 0 in
          try
            Processes.Parallel.process_stream_chunkwise
              (fun () -> if !next >= 200 then raise End_of_file else (incr next; !next))
              (fun x -> if x = 50 then failwith "item 50"; x)
              (fun _ -> Unix.sleepf 0.002)
              8
          with _ -> ()
        done;
        Unix._exit 0
      | pid -> snd (Unix.waitpid [] pid) = Unix.WEXITED 0 in
    Testing.check_bool "and workers gone while the caller asks for results do not kill it"
      ~expected:true (survives ());
    (* SIGPIPE is ignored while a section runs, so that such a write fails rather than kills *)
    Testing.check_bool "the caller's handling of SIGPIPE is the same after the section"
      ~expected:true
      (Sys.set_signal Sys.sigpipe Sys.Signal_default;
       (try failing ~slow:0 4 with _ -> ());
       Sys.signal Sys.sigpipe Sys.Signal_default = Sys.Signal_default))

(* A worker can be gone by the time the output process switches it off --
   killed from outside, say, once it has delivered all it had -- and the byte
   that switches it off then cannot be written.  That byte must not stay
   behind in the caller: every section begins by flushing every channel the
   caller has, and a byte kept by a channel whose descriptor was closed would
   be written into whatever has taken the descriptor's number by then -- a
   pipe of the next section, whose worker would then run a request ahead of
   the protocol.  Here the output function, at the last item, leaves the
   workers the time to report the end of their input and kills them; the
   section's own outcome is beside the point.  Then the lowest 64 free
   descriptor numbers, which take in every one the section had, are given to
   one file, which a flush must leave empty. *)

let test_process_stream_chunkwise_workers_gone () =
  Testing.section "Parallel streams whose workers go before they are switched off" (fun () ->
    let pids_file = Filename.temp_file "BiOCamLib_Tests_" ".pids"
    and flushed = Filename.temp_file "BiOCamLib_Tests_" ".flushed" in
    Fun.protect ~finally:(fun () -> Sys.remove pids_file; Sys.remove flushed) (fun () ->
      (* Each item notes the pid of the worker running it, in one write that a kill cannot
         cut in half, so that the output function can kill every worker *)
      let note_pid () =
        let fd = Unix.openfile pids_file [ Unix.O_WRONLY; Unix.O_APPEND; Unix.O_CREAT ] 0o644 in
        let line = Printf.sprintf "%d\n" (Unix.getpid ()) in
        ignore (Unix.write_substring fd line 0 (String.length line));
        Unix.close fd
      and kill_workers () =
        let ic = open_in pids_file in
        let rec read acc =
          match input_line ic with
          | line -> read (int_of_string line :: acc)
          | exception End_of_file -> acc in
        let pids = read [] in
        close_in ic;
        List.iter (fun pid -> try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ())
          (List.sort_uniq compare pids) in
      let next = ref 0 in
      (try
         Processes.Parallel.process_stream_chunkwise
           (fun () -> if !next >= 4 then raise End_of_file else (incr next; !next))
           (fun x -> note_pid (); x)
           (fun y -> if y = 4 then (Unix.sleepf 1.; kill_workers (); Unix.sleepf 0.5))
           2
       with Exception.E _ -> ());
      let fds = List.init 64 (fun _ -> Unix.openfile flushed [ Unix.O_WRONLY; Unix.O_APPEND ] 0) in
      flush_all ();
      List.iter Unix.close fds;
      Testing.check_int
        "a byte that could not be written to a worker gone is not written anywhere else"
        ~expected:0 (Unix.stat flushed).Unix.st_size))

(* Whether every process a section forked is gone once [sections] has returned.  They all inherit
   the write end of a pipe opened before, which reads as ended once the caller has closed its own
   copy and every one of them has gone -- which no pid taken over by another process, as the
   machine forks on, can fake. *)

let gone_after sections =
  let alive_in, alive_out = Unix.pipe () in
  match sections () with
  | res ->
    Unix.close alive_out;
    let gone =
      match Unix.select [ alive_in ] [] [] 0. with
      | [], _, _ -> false
      | _ -> Unix.read alive_in (Bytes.create 1) 0 1 = 0 in
    Unix.close alive_in;
    res, gone
  | exception e ->
    Unix.close alive_out;
    Unix.close alive_in;
    raise e

(* Stopping, looked at harder, because this function runs nearly every tool
   built on the library.  Wherever the stop falls, on any number of threads,
   what comes back is the prefix up to it, in order.  Every process the
   section forked is gone by the time it returns, whether it had finished its
   work or was killed in the middle of it, and the input process, a child of
   the caller, has been collected.  A caller that holds SIGTERM back itself
   -- the signal the section uses to stop -- can still stop a section, and
   keeps its own mask.  And many sections in a row, stopped or not, leave
   nothing behind. *)

let test_process_stream_chunkwise_stop_hard () =
  Testing.section "Parallel streams stopped early, harder" (fun () ->
    let pids_file = Filename.temp_file "BiOCamLib_Tests_" ".pids" in
    Fun.protect ~finally:(fun () -> Sys.remove pids_file) (fun () ->
      (* The input process notes its pid as it reads the first item, in one write that a kill
         cannot cut in half, so that it can be checked to have been collected afterwards *)
      let note_pid () =
        let fd = Unix.openfile pids_file [ Unix.O_WRONLY; Unix.O_APPEND; Unix.O_CREAT ] 0o644 in
        let line = Printf.sprintf "%d\n" (Unix.getpid ()) in
        ignore (Unix.write_substring fd line 0 (String.length line));
        Unix.close fd in
      (* Waiting for a child that has been collected finds no such child *)
      let collected () =
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
                match Unix.waitpid [ Unix.WNOHANG ] pid with
                | _ -> false
                | exception Unix.Unix_error (Unix.ECHILD, _, _) -> true)
              pids in
      (* Item x takes [takes x] seconds *)
      let run ?(takes = fun _ -> 0.) ~items ~stop_at threads =
        let next = ref 0 and acc = ref [] in
        Processes.Parallel.process_stream_chunkwise
          (fun () ->
            if !next = 0 then
              note_pid ();
            if !next >= items then raise End_of_file else (incr next; !next))
          (fun x -> Unix.sleepf (takes x); x)
          (fun y -> List.accum acc y; if y = stop_at then raise Processes.Parallel.Stop)
          threads;
        List.rev !acc in
      let slow k x = if x = k then 60. else 0. in
      let prefix k = List.init k (fun i -> i + 1) in
      let failures = ref [] in
      let expect what got expected =
        if got <> expected then List.accum failures what in
      List.iter
        (fun threads ->
          List.iter
            (fun stop_at ->
              let what = Printf.sprintf "stop at %d on %d threads" stop_at threads in
              let got, gone = gone_after (fun () -> run ~items:30 ~stop_at threads) in
              expect what got (prefix (min stop_at 30));
              if not (gone && collected ()) then
                List.accum failures (what ^ " left a worker"))
            [ 1; 2; 7; 29; 30; 31 ])
        [ 1; 2; 4; 16 ];
      Testing.check_string
        "every stop point, and none, on 1, 2, 4 and 16 threads: the prefix, in order, and no worker left"
        ~expected:"" (List.rev !failures |> String.concat "; ");
      let started = Unix.gettimeofday () in
      let got, gone = gone_after (fun () -> run ~takes:(slow 5) ~items:30 ~stop_at:3 4) in
      Testing.check_bool "a worker killed in the middle of an item is gone as well" ~expected:true
        (got = prefix 3 && Unix.gettimeofday () -. started < 30. && gone && collected ());
      let caller_mask = Unix.sigprocmask Unix.SIG_BLOCK [ Sys.sigterm ] in
      let started = Unix.gettimeofday () in
      let got, gone = gone_after (fun () -> run ~takes:(slow 5) ~items:30 ~stop_at:3 4) in
      let elapsed = Unix.gettimeofday () -. started in
      let still_blocked = List.mem Sys.sigterm (Unix.sigprocmask Unix.SIG_BLOCK []) in
      ignore (Unix.sigprocmask Unix.SIG_SETMASK caller_mask);
      Testing.check_bool "a caller holding SIGTERM back can still stop, and keeps its mask"
        ~expected:true (got = prefix 3 && elapsed < 30. && still_blocked && gone && collected ());
      (* Results pile up behind a slow item, as many as the section buffers, and come out a few
         at a time once it is done.  A stop among them must not wait for the next result to come
         in when every worker still at work has an item past the stop that takes a minute.  On 2
         threads the section buffers 20 results, and the other worker fills that up while item 1
         takes half a second; every item after those takes a minute *)
      let started = Unix.gettimeofday () in
      let got, gone =
        gone_after (fun () ->
          run ~takes:(fun x -> if x = 1 then 0.5 else if x > 21 then 60. else 0.) ~items:100
            ~stop_at:10 2) in
      Testing.check_bool
        "a stop among results piled up behind a slow item is not held up by items past it"
        ~expected:true
        (got = prefix 10 && Unix.gettimeofday () -. started < 30. && gone && collected ());
      failures := [];
      let (), gone =
        gone_after (fun () ->
          for i = 1 to 100 do
            let stop_at = if i mod 2 = 0 then 1 + i mod 20 else 21 in
            expect (Printf.sprintf "section %d" i) (run ~items:20 ~stop_at 4)
              (prefix (min stop_at 20))
          done) in
      Testing.check_string "a hundred sections in a row, stopped or not, are each exact"
        ~expected:"" (List.rev !failures |> String.concat "; ");
      Testing.check_bool "and leave no worker behind" ~expected:true (gone && collected ())))

(* A caller that takes a signal itself -- the SIGALRM of a timer, say -- has
   its handler run while a section waits, and the wait is interrupted.  The
   section resumes it rather than raising the interruption, and where the
   handler raises, the section ends as a stop ends it and then lets the
   exception through.  A one-shot timer, set by the output function as it
   takes the first item, goes off while the section is bound to be waiting for
   the second, which sleeps; as timers are not inherited across a fork, only
   the caller sees it.  A handler for SIGCHLD, which the input process
   inherits and which every worker's end sets off there, is tried on sections
   that end in each of the three ways. *)

let test_process_stream_chunkwise_signals () =
  Testing.section "Parallel streams in a caller that takes signals" (fun () ->
    let run ?(sleep = 0.) ?(stop_at = 0) ?(fail = 0) ?(first = ignore) threads =
      let next = ref 0 and acc = ref [] in
      Processes.Parallel.process_stream_chunkwise
        (fun () -> if !next >= 30 then raise End_of_file else (incr next; !next))
        (fun x ->
          if x = 2 then Unix.sleepf sleep;
          if x = fail then failwith "a failing item";
          x)
        (fun y ->
          if y = 1 then first ();
          List.accum acc y; if y = stop_at then raise Processes.Parallel.Stop)
        threads;
      List.rev !acc in
    let all = List.init 30 (fun i -> i + 1) in
    (* The handler given takes SIGALRM, once, 100 ms after f has set the timer through the
       function it is handed *)
    let with_alarm handler f =
      let previous = Sys.signal Sys.sigalrm (Sys.Signal_handle handler)
      and set_timer it_value =
        ignore (Unix.setitimer Unix.ITIMER_REAL { Unix.it_interval = 0.; it_value }) in
      Fun.protect ~finally:(fun () -> set_timer 0.; Sys.set_signal Sys.sigalrm previous)
        (fun () -> f (fun () -> set_timer 0.1)) in
    let fired = ref 0 in
    let got, gone =
      gone_after (fun () ->
        with_alarm (fun _ -> incr fired) (fun first -> run ~sleep:0.5 ~first 4)) in
    Testing.check_bool
      "a wait the caller's handler interrupts is resumed: every item comes back, in order"
      ~expected:true (got = all && !fired = 1);
    Testing.check_bool "and nothing is left" ~expected:true gone;
    let started = Unix.gettimeofday () in
    let raised, gone =
      gone_after (fun () ->
        match with_alarm (fun _ -> raise Exit) (fun first -> run ~sleep:60. ~first 4) with
        | _ -> false
        | exception Exit -> true) in
    Testing.check_bool
      "a handler that raises ends the section at once, and its exception comes through"
      ~expected:true (raised && Unix.gettimeofday () -. started < 30.);
    Testing.check_bool "and nothing is left either" ~expected:true gone;
    let previous = Sys.signal Sys.sigchld (Sys.Signal_handle ignore) in
    let outcomes, gone =
      gone_after (fun () ->
        Fun.protect ~finally:(fun () -> Sys.set_signal Sys.sigchld previous) (fun () ->
          List.init 15
            (fun i ->
              match i mod 3 with
              | 0 -> run 4 = all
              | 1 -> run ~stop_at:3 4 = [ 1; 2; 3 ]
              | _ ->
                match run ~fail:5 4 with
                | _ -> false
                | exception Exception.E (Exception.Kind.Algorithm, _, _) -> true))) in
    Testing.check_bool
      "with a handler for SIGCHLD, sections that end, stop or fail do as they should"
      ~expected:true (List.for_all Fun.id outcomes);
    Testing.check_bool "and leave nothing" ~expected:true gone)

(* A program that an item starts and leaves running -- or one that outlives a
   worker killed in the middle of an item -- would hold open whatever pipes of
   the section it inherited, and the section, which ends only once every
   worker's pipe has reached its end, would wait for that program to go.  So
   here the first item starts a program that lives for twenty seconds, without
   waiting for it, and the section must end well before the program would.  The
   program is killed afterwards, which is safe for as long as it is bound to be
   running still, that is, until twenty seconds have gone by. *)

let test_process_stream_chunkwise_programs () =
  Testing.section "Parallel streams whose items start programs" (fun () ->
    let pids_file = Filename.temp_file "BiOCamLib_Tests_" ".pids" in
    Fun.protect ~finally:(fun () -> Sys.remove pids_file) (fun () ->
      (* The item notes the program's pid, in one write that a kill cannot cut in half *)
      let start_program () =
        let null = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
        let pid = Unix.create_process "sleep" [| "sleep"; "20" |] null null null in
        Unix.close null;
        let fd = Unix.openfile pids_file [ Unix.O_WRONLY; Unix.O_APPEND ] 0 in
        let line = Printf.sprintf "%d\n" pid in
        ignore (Unix.write_substring fd line 0 (String.length line));
        Unix.close fd in
      let next = ref 0 and acc = ref [] in
      let started = Unix.gettimeofday () in
      Processes.Parallel.process_stream_chunkwise
        (fun () -> if !next >= 30 then raise End_of_file else (incr next; !next))
        (fun x -> if x = 1 then start_program (); x)
        (fun y -> List.accum acc y)
        4;
      let elapsed = Unix.gettimeofday () -. started in
      let ic = open_in pids_file in
      let pid = int_of_string (input_line ic) in
      close_in ic;
      if Unix.gettimeofday () -. started < 19. then
        (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
      Testing.check_bool
        "a program an item starts and leaves running does not hold the section up"
        ~expected:true (List.rev !acc = List.init 30 (fun i -> i + 1) && elapsed < 10.)))

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
  test_process_stream_chunkwise_failure ();
  test_process_stream_chunkwise_workers_gone ();
  test_process_stream_chunkwise_stop_hard ();
  test_process_stream_chunkwise_signals ();
  test_process_stream_chunkwise_programs ();
  test_process_stream_linewise ()
