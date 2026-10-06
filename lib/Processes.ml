(*
    Processes.ml -- (c) 2015-2024 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Processes.ml implements a number of tools useful to write OCaml
     programs. In particular, it contains:
     * a module to spawn and query subprocesses
     * a module to profile memory usage
     * a module to arbitrarily parallelize streams following a
        reader-workers-writer model.

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

open Better

(* Simple wrapper around Unix sub-processes *)
module Subprocess:
  sig
    (* Turn the termination status of a subprocess into either unit or an exception naming the
        command and what went wrong. Exposed because a process spawned elsewhere - Files.Compressed
        hands its channel to the caller and is reaped much later - still has to classify its exit
        the same way, and two ways of deciding that a helper failed would eventually disagree.
       ~kind is mandatory, and deliberately has no default: no single kind is right for every
        caller, so each states what a failure there means - Subprocess for a command that simply
        did not work, IO_Format where its failure says something about the data.
       stderr_contents, when not empty, is echoed before raising, so the helper's own diagnosis
        ("unexpected end of file") reaches the user ahead of ours *)
    val handle_termination_status:
      kind:Exception.Kind.t -> string -> string -> Unix.process_status -> unit
    (* It is possible for all these functions to fail *)
    (* Execute a simple command - the executables do not need to be a fully qualified path *)
    val spawn: ?verbose:bool -> string -> unit
    val spawn_and_read_single_line: ?verbose:bool -> string -> string
    (* The following two functions spawn a subprocess, with its output being read by the parent.
       The second function argument processes the output line-wise.
       THE EXECUTABLES MUST BE FULLY QUALIFIED PATHS *)
    val spawn_and_process_output: ?verbose:bool ->
      (unit -> unit) -> (int -> string -> unit) -> (unit -> unit) -> string -> unit
    val spawn_with_args_and_process_output: ?verbose:bool ->
      (unit -> unit) -> (int -> string -> unit) -> (unit -> unit) -> string -> string array -> unit
  end
= struct
    (* PUBLIC, although the functions below are its main users *)
    let handle_termination_status ~kind command stderr_contents status =
      let raise_command_failed problem =
        Exception.raise __FUNCTION__ kind (Printf.sprintf "Command '%s' failed (%s)" command problem) in
      match status with
      | Unix.WEXITED 0 -> ()
      | e ->
        Printf.eprintf "%s%!" stderr_contents;
        match e with
        | WEXITED n ->
          raise_command_failed (Printf.sprintf "Process exit status was %d" n)
        | WSIGNALED n ->
          raise_command_failed (Printf.sprintf "Process killed by signal %d" n)
        | WSTOPPED n ->
          raise_command_failed (Printf.sprintf "Process stopped by signal %d" n)
    (* PUBLIC *)
    let spawn ?(verbose = false) command =
      if verbose then
        Printf.eprintf "Subprocess.spawn: Executing command '%s'...\n%!" command;
      Unix.system command |> handle_termination_status ~kind:(Subprocess command) command ""
    let spawn_and_read_single_line ?(verbose = false) command =
      if verbose then
        Printf.eprintf "Subprocess.spawn_and_read_single_line: Executing command '%s'...\n%!" command;
      let process_out = Unix.open_process_in command in
      let res =
        try
          input_line process_out
        with End_of_file -> "" in
      Unix.close_process_in process_out |> handle_termination_status ~kind:(Subprocess command) command "";
      res
    let spawn_with_args_and_process_output ?(verbose = false) pre f post command args =
      if verbose then begin
        let command = Array.fold_left (fun accum arg -> accum ^ " " ^ arg) command args in
        Printf.eprintf "Subprocess.spawn_with_args_and_process_output: Executing command '%s'...\n%!" command
      end;
      let process_out, process_in, process_err =
        Unix.unsafe_environment () |> Unix.open_process_args_full command args in
      close_out process_in;
      if verbose then
        Printf.eprintf "Subprocess.spawn_with_args_and_process_output: Executing initialization function...\n%!";
      pre ();
      if verbose then
        Printf.eprintf "Subprocess.spawn_with_args_and_process_output: Processing contents of standard output...\n%!";
      begin try
        let line_cntr = ref 0 in
        while true do
          let line = input_line process_out in
          incr line_cntr;
          f !line_cntr line
        done
      with End_of_file -> ()
      end;
      if verbose then
        Printf.eprintf "Subprocess.spawn_with_args_and_process_output: Executing finalization function...\n%!";
      post ();
      (* There might be content on the stderr - we collect it here, and output it in case of error *)
      if verbose then
        Printf.eprintf "Subprocess.spawn_with_args_and_process_output: Collecting contents of standard error...\n%!";
      let stderr_contents = Buffer.create 1024 in
      begin try
        while true do
          input_line process_err |> Buffer.add_string stderr_contents;
          Buffer.add_char stderr_contents '\n'
        done
      with End_of_file -> ()
      end;
      close_in process_out;
      close_in process_err;
      Unix.close_process_full (process_out, process_in, process_err) |>
        handle_termination_status ~kind:(Subprocess command) command (Buffer.contents stderr_contents)
    let spawn_and_process_output ?(verbose = false) pre f post command =
      spawn_with_args_and_process_output ~verbose pre f post command [||]
  end

module Memory:
  sig
    (* Quick-n-dirty hook to get how much memory a process is using *)
    val get_rs_size: unit -> float
    val get_gc_size: unit -> float
    module Profiler:
      sig
        type t = {
          sampling_rate: int;
          current_rs_size: float;
          maximum_rs_size: float;
          current_gc_size: float;
          maximum_gc_size: float
        }
        val make: ?sampling_rate:int -> unit -> t
        val update: t -> t
      end
  end
= struct
    (* An empty header -- '-o rss=' -- is how the header line is suppressed
       portably: procps also accepts '--no-headers', and BSD ps, which is what
       macOS has, refuses it and exits 1, so the resident size could not be read
       there at all *)
    let get_rs_size () =
      Unix.getpid () |> Printf.sprintf "ps -p %d -o rss=" |>
          Subprocess.spawn_and_read_single_line |> float_of_string
    let get_gc_size () =
      (Gc.stat ()).major_words *. 8.
    module Profiler =
      struct
        type t = {
          sampling_rate: int;
          current_rs_size: float;
          maximum_rs_size: float;
          current_gc_size: float;
          maximum_gc_size: float
        }
        let make ?(sampling_rate = 100) () =
          { sampling_rate;
            current_rs_size = 0.;
            maximum_rs_size = 0.;
            current_gc_size = 0.;
            maximum_gc_size = 0. }
        let update p =
          if Random.int p.sampling_rate = 0 then begin
            let current_rs_size = get_rs_size ()
            and current_gc_size = get_gc_size () in
            { p with
              current_rs_size;
              maximum_rs_size = max current_rs_size p.current_rs_size;
              current_gc_size;
              maximum_gc_size = max current_gc_size p.current_gc_size }
          end else
            p
      end
  end

module Parallel:
  sig
    val get_nproc: unit -> int
    (* Raised by the output function of process_stream_chunkwise to end the section early: no
       further result is delivered, the processes the section forked are killed and collected,
       and the function returns as it does at the end of the stream *)
    exception Stop
    (* The following functions can fail if the number of chunks/threads is not positive *)
    val process_stream_chunkwise: ?buffered_chunks_per_thread:int ->
      (* Beware: for everything to terminate properly, f shall raise End_of_file when done.
         Side effects are propagated within f (not exported) and within h (exported).
         Where g raises, or a worker goes without delivering its result, the section is ended
         as Stop ends it, and an Algorithm exception is raised once it has been.  Anything else
         raised while the section runs -- by h, or by a handler of the caller's for a signal --
         ends it the same way, and is raised again once it has been *)
      (unit -> 'a) -> ('a -> 'b) -> ('b -> unit) -> int -> unit
    val process_stream_linewise: ?buffered_chunks_per_thread:int -> ?max_memory:int -> ?string_buffer_memory:int ->
                                 ?input_line:(in_channel -> string) -> ?verbose:bool ->
      in_channel -> (Buffer.t -> int -> string -> unit) -> out_channel -> int -> unit
  end
= struct
    exception Stop
    (* What the output process makes of a worker that failed or went, to end the section by *)
    exception Worker_failed
    let get_nproc () =
      try
        (* nproc is GNU, and a Mac has none: there sysctl answers instead.  The command
           goes through /bin/sh, so the fallback is asked for in the command itself, and
           both are silenced so that the one that is missing says nothing on stderr *)
        Subprocess.spawn_and_read_single_line "nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null"
        |> int_of_string
      with _ ->
        1
    let process_stream_chunkwise ?(buffered_chunks_per_thread = 10)
        (f:unit -> 'a) (g:'a -> 'b) (h:'b -> unit) threads =
      if buffered_chunks_per_thread < 1 || threads < 1 then
        Exception.raise __FUNCTION__ Initialize
          (Printf.sprintf "Number of chunks per thread and number of threads must be positive (found %d, %d)"
            buffered_chunks_per_thread threads);
      let red_threads = threads - 1 in
      (* I am the ouptut process *)
      let close_pipe (pipe_in, pipe_out) = Unix.close pipe_in; Unix.close pipe_out in
      let close_pipes_in = Array.iter (fun (pipe_in, _) -> Unix.close pipe_in)
      and close_pipes_out = Array.iter (fun (_, pipe_out) -> Unix.close pipe_out)
      and close_pipes = Array.iter close_pipe
      and get_stuff_for_select pipes =
        let pipes = Array.map fst pipes in
        Array.to_list pipes, begin
          let dict = Hashtbl.create (Array.length pipes) in
          Array.iteri
            (fun i pipe ->
               assert (not (Hashtbl.mem dict pipe));
               Hashtbl.add dict pipe i)
            pipes;
          dict
        end
      (* EVERY PIPE OF THE SECTION IS CLOSED ON EXEC.  The processes forked here keep their ends,
         and a program that f, g or h starts does not get them: one left running would otherwise
         hold a worker's pipe open, and the section, which ends once every worker's pipe has
         reached its end, would wait for it *)
      and w_2_o_pipes = Array.init threads (fun _ -> Unix.pipe ~cloexec:true ())
      and o_2_w_pipes = Array.init threads (fun _ -> Unix.pipe ~cloexec:true ()) in
      (* A WAIT THAT A SIGNAL INTERRUPTS IS RESUMED, in every process of the section.  A caller
         with a handler of its own -- for SIGCHLD, say, or for SIGALRM from a timer -- would
         otherwise have the interruption raised out of the section, and the input process inherits
         that handler too.  The handler has run by the time the wait is left, and one that raises
         sends its own exception on instead *)
      let rec select_readable ?(timeout = -1.) pipes =
        try
          let ready, _, _ = Unix.select pipes [] [] timeout in
          ready
        with Unix.Unix_error (Unix.EINTR, _, _) ->
          select_readable ~timeout pipes in
      let rec collect pid =
        try
          ignore (Unix.waitpid [] pid)
        with
        | Unix.Unix_error (Unix.EINTR, _, _) -> collect pid
        | Unix.Unix_error _ -> () in
      (* What the caller has written and not yet flushed would otherwise be in every process forked
         from here, and written again by any of them that writes *)
      flush_all ();
      (* A WORKER CAN GO WHILE THE OUTPUT PROCESS IS WRITING TO IT -- killed by the input process
         once another one has failed, or from outside for the memory it took -- and the write
         would then raise SIGPIPE, which by default ends the caller's whole program without a
         word.  The signal is ignored for as long as the section runs, so that such a write fails
         instead and ends the section as a failed worker does.  The input process keeps it ignored,
         so that a write of its own to a worker gone ends the section too.  The workers run the
         caller's code with the caller's own handling of it, which is put back here when the
         section ends *)
      let previous_sigpipe = Sys.signal Sys.sigpipe Sys.Signal_ignore in
      (* SIGTERM IS HELD BACK ACROSS THE FORK.  It is what stops the section from here, and it may
         be sent as soon as the fork has returned -- by a handler of the caller's that raises out of
         the first wait, say.  Held back, it waits for the input process to have a handler of its
         own, rather than meeting there whatever the caller does with the signal: a caller that
         ignores it would leave the input process running, and this one waiting for it for ever *)
      let caller_mask = Unix.sigprocmask Unix.SIG_BLOCK [ Sys.sigterm ] in
      match Unix.fork () with
      | 0 -> (* Child *)
        (* I am the input process *)
        let workers = ref [] in
        (* A SECTION STOPPED EARLY IS ENDED FROM HERE. The output process cannot reach the workers,
           which are children of this process, so it tells this process, and this process kills
           and collects every worker it forked before going itself.  The signal comes in held
           back, and is let through once every worker has been forked, so that none can be forked
           after the handler has looked for them and be left running.  It is held back again
           before the first worker is collected, so that the handler never signals a worker that
           has been collected already, and whose pid may by then be another process's *)
        let previous_handler =
          Sys.signal Sys.sigterm
            (Sys.Signal_handle
              (fun _ ->
                List.iter (fun pid -> try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ())
                  !workers;
                List.iter collect !workers;
                Unix._exit 0)) in
        (* NOTHING THIS PROCESS MEETS IS LET OUT OF IT: an exception let through would carry it out
           of the section and on into the caller's code, running it a second time.  A worker that
           cannot be forked, a worker gone, which leaves its pipe at its end, or a reader raising
           what is not the end of its input, ends the section from here, the workers being killed
           and collected first -- whether or not the message saying why can be written *)
        begin try
          (* Closed on exec too, as the output process's are *)
          let i_2_w_pipes = Array.init threads (fun _ -> Unix.pipe ~cloexec:true ())
          and w_2_i_pipes = Array.init threads (fun _ -> Unix.pipe ~cloexec:true ()) in
          for i = 0 to red_threads do
            match Unix.fork () with
            | 0 -> (* Child *)
              (* I am a worker.
                 NOTHING THIS PROCESS MEETS IS LET OUT OF IT either, not even into the code of the
                 input process it was forked in.  A chunk that raises is reported to the output
                 process in the place of a notification, which ends the section; anything else --
                 the input or the output process gone, say -- ends this process, which the output
                 process then sees *)
              begin try
                (* The handlers above are my parents' business: I run the caller's code, and get
                   back exactly the handling of the signals the caller had *)
                Sys.set_signal Sys.sigterm previous_handler;
                Sys.set_signal Sys.sigpipe previous_sigpipe;
                ignore (Unix.sigprocmask Unix.SIG_SETMASK caller_mask);
                (* I only keep my own pipes open *)
                let i_2_w_pipe_in, w_2_i_pipe_out, o_2_w_pipe_in, w_2_o_pipe_out =
                  let i_2_w_pipe_in = ref Unix.stdin and w_2_i_pipe_out = ref Unix.stdout
                  and o_2_w_pipe_in = ref Unix.stdin and w_2_o_pipe_out = ref Unix.stdout in
                  for ii = 0 to red_threads do
                    if ii = i then begin
                      let pipe_in, pipe_out = i_2_w_pipes.(ii) in
                      i_2_w_pipe_in := pipe_in;
                      Unix.close pipe_out;
                      let pipe_in, pipe_out = w_2_i_pipes.(ii) in
                      Unix.close pipe_in;
                      w_2_i_pipe_out := pipe_out;
                      let pipe_in, pipe_out = o_2_w_pipes.(ii) in
                      o_2_w_pipe_in := pipe_in;
                      Unix.close pipe_out;
                      let pipe_in, pipe_out = w_2_o_pipes.(ii) in
                      Unix.close pipe_in;
                      w_2_o_pipe_out := pipe_out
                    end else begin
                      close_pipe i_2_w_pipes.(ii);
                      close_pipe w_2_i_pipes.(ii);
                      close_pipe o_2_w_pipes.(ii);
                      close_pipe w_2_o_pipes.(ii)
                    end
                  done;
                  !i_2_w_pipe_in, !w_2_i_pipe_out, !o_2_w_pipe_in, !w_2_o_pipe_out in
                let i_2_w = Unix.in_channel_of_descr i_2_w_pipe_in
                and w_2_i = Unix.out_channel_of_descr w_2_i_pipe_out
                and o_2_w = Unix.in_channel_of_descr o_2_w_pipe_in
                and w_2_o = Unix.out_channel_of_descr w_2_o_pipe_out in
                (* My protocol is:
                   (1) process a chunk more from the input
                   (2) notify the output process that a result is ready
                   (3) when the output process asks for it, post the result *)
                let probe_output () =
                  ignore (input_byte o_2_w)
                and initial = ref true in
                while true do
                  (* Try to get one more chunk.
                     Signal the input process that I am idle *)
                  output_byte w_2_i 0;
                  flush w_2_i;
                  (* Get & process a chunk *)
                  match input_byte i_2_w with
                  | 0 -> (* EOF reached *)
                    (* Did the output process ask for a notification? *)
                    if not !initial then (* The first time, we notify anyway to avoid crashes *)
                      probe_output ();
                    (* Notify that EOF has been reached *)
                    output_binary_int w_2_o (-1);
                    flush w_2_o;
                    (* Did the output process switch me off? *)
                    probe_output ();
                    (* Commit suicide *)
                    Unix.close i_2_w_pipe_in;
                    Unix.close w_2_i_pipe_out;
                    Unix.close o_2_w_pipe_in;
                    Unix.close w_2_o_pipe_out;
                    Unix._exit 0 (* Do not flush buffers or do anything else *)
                  | 1 -> (* OK, one more token available *)
                    (* Get the chunk *)
                    let chunk_id, data = (input_value i_2_w:int * 'a) in
                    (* Process the chunk *)
                    let data =
                      try
                        g data
                      with e ->
                        Printf.eprintf "(%s): a worker failed: %s\n%!" __FUNCTION__
                          (Printexc.to_string e);
                        if not !initial then
                          probe_output ();
                        output_binary_int w_2_o (-2);
                        flush w_2_o;
                        Unix._exit 1 in
                    (* Did the output process ask for a notification? *)
                    if not !initial then (* The first time, we notify anyway to avoid crashes *)
                      probe_output ()
                    else
                      initial := false;
                    (* Tell the output process what we have *)
                    output_binary_int w_2_o chunk_id;
                    flush w_2_o;
                    (* Did the output process request data? *)
                    probe_output ();
                    (* Send the data to output *)
                    output_value w_2_o data;
                    flush w_2_o
                  | _ -> assert false
                done
              with e ->
                (try
                   Printf.eprintf "(%s): a worker went: %s\n%!" __FUNCTION__ (Printexc.to_string e)
                 with _ -> ());
                Unix._exit 1
              end
            | worker_pid -> (* Parent *)
              workers := worker_pid :: !workers
          done;
          (* Every worker is known now, so a request to stop can be taken -- even where the caller
             holds the signal back itself, this process being the section's and not the caller's *)
          ignore (Unix.sigprocmask Unix.SIG_SETMASK (List.filter (( <> ) Sys.sigterm) caller_mask));
          (* I am the input process.
             I do not care about output process pipes *)
          close_pipes w_2_o_pipes;
          close_pipes o_2_w_pipes;
          close_pipes_in i_2_w_pipes;
          close_pipes_out w_2_i_pipes;
          let w_2_i_pipes_for_select, w_2_i_dict = get_stuff_for_select w_2_i_pipes
          and w_2_i = Array.map (fun (pipe_in, _) -> Unix.in_channel_of_descr pipe_in) w_2_i_pipes
          and i_2_w =
            Array.map (fun (_, pipe_out) -> Unix.out_channel_of_descr pipe_out) i_2_w_pipes in
          (* My protocol is:
             (1) read a chunk
             (2) read a thread id
             (3) post the chunk to the correspondng pipe. The worker will consume it *)
          let chunk_id = ref 0 and off = ref 0 in
          while !off < threads do
            let ready = select_readable w_2_i_pipes_for_select in
            List.iter
              (fun ready ->
                let w_id = Hashtbl.find w_2_i_dict ready in
                ignore (input_byte w_2_i.(w_id));
                let i_2_w = i_2_w.(w_id) in
                try
                  if !off > 0 then
                    raise End_of_file;
                  let payload = f () in
                  output_byte i_2_w 1; (* OK to transmit, we have not reached EOF yet *)
                  flush i_2_w;
                  output_value i_2_w (!chunk_id, payload);
                  flush i_2_w;
                  incr chunk_id
                with End_of_file ->
                  output_byte i_2_w 0; (* Nothing to transmit *)
                  flush i_2_w;
                  incr off)
              ready
          done;
          (* Every worker has been sent the end of its input, and goes once the output process has
             switched it off, or once it has been killed if the section is stopped.  Nothing comes
             down the pipe of a worker after the request that the end answered, so the pipe can
             only be read at its end, which it reaches when the worker has gone *)
          let running = ref w_2_i_pipes_for_select in
          while !running <> [] do
            let ready = select_readable !running in
            List.iter
              (fun ready ->
                match input_byte w_2_i.(Hashtbl.find w_2_i_dict ready) with
                | _ -> assert false
                | exception End_of_file -> ())
              ready;
            running := List.filter (fun pipe -> not (List.mem pipe ready)) !running
          done
        with e ->
          ignore (Unix.sigprocmask Unix.SIG_BLOCK [ Sys.sigterm ]);
          (try
             Printf.eprintf "(%s): the input process of a parallel section stopped: %s\n%!"
               __FUNCTION__ (Printexc.to_string e)
           with _ -> ());
          List.iter (fun pid -> try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ())
            !workers;
          List.iter collect !workers;
          Unix._exit 1
        end;
        (* THE WORKERS ARE COLLECTED BEFORE THIS PROCESS GOES, each by its own pid rather than by
           waiting for whatever turns up: a caller of this function may have children of its own
           that it means to reap itself, and an indiscriminate wait would take one of those *)
        ignore (Unix.sigprocmask Unix.SIG_BLOCK [ Sys.sigterm ]);
        List.iter collect !workers;
        Unix._exit 0 (* Do not flush buffers or do anything else *)
      | input_pid -> (* I am the output process *)
        close_pipes_in o_2_w_pipes;
        close_pipes_out w_2_o_pipes;
        let w_2_o_pipes_for_select, w_2_o_dict = get_stuff_for_select w_2_o_pipes
        and w_2_o = Array.map (fun (pipe_in, _) -> Unix.in_channel_of_descr pipe_in) w_2_o_pipes
        and o_2_w = Array.map (fun (_, pipe_out) -> Unix.out_channel_of_descr pipe_out) o_2_w_pipes
        and buffered_chunks = buffered_chunks_per_thread * threads
        and next = ref 0 and queue = ref IntMap.empty and buf = ref IntMap.empty
        and off = ref 0 in
        (* A request to a worker that has gone fails, SIGPIPE being ignored, and ends the section *)
        let ask w_id =
          try
            output_byte o_2_w.(w_id) 0;
            flush o_2_w.(w_id)
          with Sys_error _ ->
            raise_notrace Worker_failed in
        (* The caller may stop the section from h, in which case it is left at once, whatever
           is still being processed. A worker that failed, or that went without a word -- killed,
           say, for the memory it took -- ends it the same way, and then the caller hears of it.
           So does anything else raised here -- by h, or by a handler of the caller's for a
           signal -- which is raised again once the section has ended *)
        let stopped, raised =
          try
            (* The caller's own handling of SIGTERM is back for as long as the section runs *)
            ignore (Unix.sigprocmask Unix.SIG_SETMASK caller_mask);
            while !off < threads do
              (* Harvest new notifications.  THEY ARE NOT WAITED FOR WHILE THERE IS SOMETHING TO
                 FETCH OR TO OUTPUT ALREADY: every worker free to notify may be waiting to be asked
                 for what it has, and the wait would then last as long as the one still busy -- on
                 an item past where the caller stops, say -- holding back all that is ready *)
              let can_fetch =
                !queue <> IntMap.empty
                && (IntMap.cardinal !buf < buffered_chunks
                    || fst (IntMap.min_binding !queue) = !next)
              and can_output = !buf <> IntMap.empty && fst (IntMap.min_binding !buf) = !next in
              let ready =
                select_readable ~timeout:(if can_fetch || can_output then 0. else -1.)
                  w_2_o_pipes_for_select in
              List.iter
                (fun ready ->
                  let w_id = Hashtbl.find w_2_o_dict ready in
                  let chunk_id = try input_binary_int w_2_o.(w_id) with End_of_file -> -2 in
                  if chunk_id = -2 then
                    raise_notrace Worker_failed
                  else if chunk_id = -1 then (* EOF has been reached *)
                    incr off
                  else
                    if not (IntMap.mem chunk_id !queue) then
                      queue := IntMap.add chunk_id w_id !queue
                    else
                      assert (w_id = IntMap.find chunk_id !queue))
                ready;
              (* Fill the buffer *)
              let available = ref (buffered_chunks - IntMap.cardinal !buf) in
              assert (!available >= 0);
              (* If the needed chunk is there, we always fetch it *)
              if !queue <> IntMap.empty && fst (IntMap.min_binding !queue) = !next then
                incr available;
              while !available > 0 && !queue <> IntMap.empty do
                let chunk_id, w_id = IntMap.min_binding !queue in
                (* Tell the worker to send data *)
                ask w_id;
                assert (not (IntMap.mem chunk_id !buf));
                let data =
                  try
                    (input_value w_2_o.(w_id):'b)
                  with End_of_file | Failure _ -> (* Gone before sending it, or while doing so *)
                    raise_notrace Worker_failed in
                buf := IntMap.add chunk_id data !buf;
                (* Tell the worker to send the next notification *)
                ask w_id;
                queue := IntMap.remove chunk_id !queue;
                decr available
              done;
              (* Output at most as many chunks at the number of workers *)
              available := threads;
              while !available > 0 && !buf <> IntMap.empty do
                let chunk_id, data = IntMap.min_binding !buf in
                if chunk_id = !next then begin
                  h data;
                  buf := IntMap.remove chunk_id !buf;
                  incr next;
                  decr available
                end else
                  available := 0 (* Force exit from the cycle *)
              done
            done;
            (* There might be chunks left in the buffer *)
            while !buf <> IntMap.empty do
              let chunk_id, data = IntMap.min_binding !buf in
              assert (chunk_id = !next);
              h data;
              buf := IntMap.remove chunk_id !buf;
              incr next
            done;
            false, None
          with
          | Stop -> true, None
          | e -> true, Some (e, Printexc.get_raw_backtrace ()) in
        (* THE PIPES TO AND FROM THE WORKERS ARE CLOSED AS CHANNELS, which drops whatever a request
           to a worker gone could not deliver.  With only its descriptor closed, a channel would
           keep that byte, and the flush_all that every section begins with would write it into
           whatever has taken the descriptor's number by then -- a pipe of the next section, say,
           whose worker would then run a request ahead of the protocol and go before its end,
           which ends that section as a worker gone *)
        let close_channels () =
          Array.iter close_out_noerr o_2_w;
          Array.iter close_in_noerr w_2_o in
        (* AND THE INPUT PROCESS IS COLLECTED HERE.  Every call forks one child from this side,
           and a caller that opens a parallel section per unit of work rather than once per run
           -- the Monte-Carlo clusterer opens one per epoch -- would otherwise fill the process
           table with them.  A wait a signal interrupts is resumed, as it must be before a
           stopped section's pipes are closed *)
        if stopped then begin
          (* The input process kills and collects its workers, and then goes.
             THE PIPES STAY OPEN UNTIL IT HAS GONE: a worker waiting on one that closed would read
             the end of its input, and the exception would carry it out of its loop and on into
             the caller's code, running it a second time *)
          (try Unix.kill input_pid Sys.sigterm with Unix.Unix_error _ -> ());
          collect input_pid;
          close_channels ();
          Sys.set_signal Sys.sigpipe previous_sigpipe;
          match raised with
          | Some (Worker_failed, _) ->
            Exception.raise __FUNCTION__ Algorithm
              "A worker of the parallel section failed, or went without delivering its result"
          | Some (e, backtrace) -> Printexc.raise_with_backtrace e backtrace
          | None -> ()
        end else begin
          (* Switch off all the workers.  One gone since it delivered the end of its input needs
             no switching off *)
          for ii = 0 to red_threads do
            try
              output_byte o_2_w.(ii) 0;
              flush o_2_w.(ii)
            with Sys_error _ -> ()
          done;
          close_channels ();
          collect input_pid;
          Sys.set_signal Sys.sigpipe previous_sigpipe
        end
      | exception e ->
        (* A fork that fails leaves no section to end: what was set up for it is undone *)
        ignore (Unix.sigprocmask Unix.SIG_SETMASK caller_mask);
        close_pipes w_2_o_pipes;
        close_pipes o_2_w_pipes;
        Sys.set_signal Sys.sigpipe previous_sigpipe;
        raise e
    let process_stream_linewise ?(buffered_chunks_per_thread = 10)
        ?(max_memory = 1_000_000_000) ?(string_buffer_memory = 16_777_216)
        ?(input_line = input_line) ?(verbose = true)
        input (f:Buffer.t -> int -> string -> unit) output threads =
      let max_block_bytes = max_memory / (buffered_chunks_per_thread * threads) in
      (* Parallel section *)
      let read = ref 0 and eof_reached = ref false
      and processing_buffer = Buffer.create string_buffer_memory and processed = ref 0
      and written = ref 0 in
      if verbose then
        Printf.teprintf "0 lines read\n";
      process_stream_chunkwise ~buffered_chunks_per_thread:buffered_chunks_per_thread
        (fun () ->
          if not !eof_reached then begin
            let bytes = ref 0 and read_base = !read and buf = ref [] in
            begin try
              while !bytes < max_block_bytes do (* We read at least one line *)
                let line = input_line input in
                List.accum buf line;
                bytes := String.length line + !bytes;
                incr read
              done
            with End_of_file ->
              eof_reached := true
            end;
            if verbose then
              Printf.teprintf "%d %s read\n" !read (String.pluralize_int "line" !read);
            read_base, !read - read_base, !buf
          end else
            raise End_of_file)
        (fun (lines_base, lines, buf) ->
          Buffer.clear processing_buffer;
          processed := 0;
          List.iter
            (fun line ->
              f processing_buffer (lines_base + !processed) line;
              incr processed)
            (List.rev buf);
          assert (!processed = lines);
          if verbose then
            Printf.teprintf "%d more %s processed\n" lines (String.pluralize_int "line" lines);
          lines_base, lines, Buffer.contents processing_buffer)
        (fun (_, buf_len, buf) ->
          written := !written + buf_len;
          Printf.fprintf output "%s%!" buf;
          if verbose then
            Printf.teprintf "%d %s written\n" !written (String.pluralize_int "line" !written))
        threads;
      flush output;
      if verbose then
        Printf.teprintf "%d %s out\n" !written (String.pluralize_int "line" !written)
  end

