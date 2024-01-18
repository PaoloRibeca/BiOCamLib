(*
    Processes.ml -- (c) 2015-2024 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Processes.ml implements a number of tools useful to implement OCaml
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

(* Simple wrapper around Unix sub-processes *)
module Subprocess:
  sig
    exception Some_problem_occurred of string * string
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
    exception Some_problem_occurred of string * string
    (* PRIVATE *)
    let handle_termination_status command stderr_contents = function
      | Unix.WEXITED 0 -> ()
      | e ->
        Tools.Printf.eprintf "%s%!" stderr_contents;
        match e with
        | WEXITED n ->
          Some_problem_occurred (command, Tools.Printf.sprintf "Process exit status was %d" n) |> raise
        | WSIGNALED n ->
          Some_problem_occurred (command, Tools.Printf.sprintf "Process killed by signal %d" n) |> raise
        | WSTOPPED n ->
          Some_problem_occurred (command, Tools.Printf.sprintf "Process stopped by signal %d" n) |> raise
    (* PUBLIC *)
    let spawn ?(verbose = false) command =
      if verbose then
        Tools.Printf.eprintf "Subprocess.spawn: Executing command '%s'...\n%!" command;
      Unix.system command |> handle_termination_status command ""
    let spawn_and_read_single_line ?(verbose = false) command =
      if verbose then
        Tools.Printf.eprintf "Subprocess.spawn_and_read_single_line: Executing command '%s'...\n%!" command;
      let process_out = Unix.open_process_in command in
      let res =
        try
          input_line process_out
        with End_of_file -> "" in
      Unix.close_process_in process_out |> handle_termination_status command "";
      res
    let spawn_with_args_and_process_output ?(verbose = false) pre f post command args =
      if verbose then begin
        let command = Array.fold_left (fun accum arg -> accum ^ " " ^ arg) command args in
        Tools.Printf.eprintf "Subprocess.spawn_with_args_and_process_output: Executing command '%s'...\n%!" command
      end;
      let process_out, process_in, process_err =
        Unix.unsafe_environment () |> Unix.open_process_args_full command args in
      close_out process_in;
      if verbose then
        Tools.Printf.eprintf "Subprocess.spawn_with_args_and_process_output: Executing initialization function...\n%!";
      pre ();
      if verbose then
        Tools.Printf.eprintf "Subprocess.spawn_with_args_and_process_output: Processing contents of standard output...\n%!";
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
        Tools.Printf.eprintf "Subprocess.spawn_with_args_and_process_output: Executing finalization function...\n%!";
      post ();
      (* There might be content on the stderr - we collect it here, and output it in case of error *)
      if verbose then
        Tools.Printf.eprintf "Subprocess.spawn_with_args_and_process_output: Collecting contents of standard error...\n%!";
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
        handle_termination_status command (Buffer.contents stderr_contents)
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
    let get_rs_size () =
      Unix.getpid () |> Printf.sprintf "ps -p %d -o rss --no-headers" |>
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
    exception Number_of_chunks_must_be_positive of int
    exception Number_of_threads_must_be_positive of int
    val process_stream_chunkwise: ?buffered_chunks_per_thread:int ->
      (* Beware: for everything to terminate properly, f shall raise End_of_file when done.
         Side effects are propagated within f (not exported) and within h (exported) *)
      (unit -> 'a) -> ('a -> 'b) -> ('b -> unit) -> int -> unit
    val process_stream_linewise: ?buffered_chunks_per_thread:int -> ?max_memory:int -> ?string_buffer_memory:int ->
                                 ?input_line:(in_channel -> string) -> ?verbose:bool ->
      in_channel -> (Buffer.t -> int -> string -> unit) -> out_channel -> int -> unit
  end
= struct
    let get_nproc () =
      try
        Subprocess.spawn_and_read_single_line "nproc" |> int_of_string
      with _ ->
        1
    exception Number_of_chunks_must_be_positive of int
    exception Number_of_threads_must_be_positive of int
    let process_stream_chunkwise ?(buffered_chunks_per_thread = 10)
        (f:unit -> 'a) (g:'a -> 'b) (h:'b -> unit) threads =
      if buffered_chunks_per_thread < 1 then
        Number_of_chunks_must_be_positive buffered_chunks_per_thread |> raise;
      if threads < 1 then
        Number_of_threads_must_be_positive threads |> raise;
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
      and w_2_o_pipes = Array.init threads (fun _ -> Unix.pipe ())
      and o_2_w_pipes = Array.init threads (fun _ -> Unix.pipe ()) in
      match Unix.fork () with
      | 0 -> (* Child *)
        (* I am the input process *)
        let i_2_w_pipes = Array.init threads (fun _ -> Unix.pipe ())
        and w_2_i_pipes = Array.init threads (fun _ -> Unix.pipe ()) in
        for i = 0 to red_threads do
          match Unix.fork () with
          | 0 -> (* Child *)
            (* I am a worker.
               I only keep my own pipes open *)
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
              (*ignore (Unix.select [o_2_w_pipe_in] [] [] (-1.));*)
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
                let data = g data in
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
          | _ -> () (* Parent *)
        done;
        (* I am the input process.
           I do not care about output process pipes *)
        close_pipes w_2_o_pipes;
        close_pipes o_2_w_pipes;
        close_pipes_in i_2_w_pipes;
        close_pipes_out w_2_i_pipes;
        let w_2_i_pipes_for_select, w_2_i_dict = get_stuff_for_select w_2_i_pipes
        and w_2_i = Array.map (fun (pipe_in, _) -> Unix.in_channel_of_descr pipe_in) w_2_i_pipes
        and i_2_w = Array.map (fun (_, pipe_out) -> Unix.out_channel_of_descr pipe_out) i_2_w_pipes in
        (* My protocol is:
           (1) read a chunk
           (2) read a thread id
           (3) post the chunk to the correspondng pipe. The worker will consume it *)
        let chunk_id = ref 0 and off = ref 0 in
        while !off < threads do
          let ready, _, _ = Unix.select w_2_i_pipes_for_select [] [] (-1.) in
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
        (* Waiting to be switched off *)
        ignore (Unix.select [fst w_2_i_pipes.(0)] [] [] (-1.));
        close_pipes_out i_2_w_pipes;
        close_pipes_in w_2_i_pipes;
        Unix._exit 0 (* Do not flush buffers or do anything else *)
      | _ -> (* I am the output process *)
        close_pipes_in o_2_w_pipes;
        close_pipes_out w_2_o_pipes;
        let w_2_o_pipes_for_select, w_2_o_dict = get_stuff_for_select w_2_o_pipes
        and w_2_o = Array.map (fun (pipe_in, _) -> Unix.in_channel_of_descr pipe_in) w_2_o_pipes
        and o_2_w = Array.map (fun (_, pipe_out) -> Unix.out_channel_of_descr pipe_out) o_2_w_pipes
        and buffered_chunks = buffered_chunks_per_thread * threads
        and next = ref 0 and queue = ref Tools.IntMap.empty and buf = ref Tools.IntMap.empty
        and off = ref 0 in
        while !off < threads do
          (* Harvest new notifications *)
          let ready, _, _ = Unix.select w_2_o_pipes_for_select [] [] (-1.) in
          List.iter
            (fun ready ->
              let w_id = Hashtbl.find w_2_o_dict ready in
              let chunk_id = input_binary_int w_2_o.(w_id) in
              if chunk_id = -1 then (* EOF has been reached *)
                incr off
              else
                if not (Tools.IntMap.mem chunk_id !queue) then
                  queue := Tools.IntMap.add chunk_id w_id !queue
                else
                  assert (w_id = Tools.IntMap.find chunk_id !queue))
            ready;
          (* Fill the buffer *)
          let available = ref (buffered_chunks - Tools.IntMap.cardinal !buf) in
          assert (!available >= 0);
          (* If the needed chunk is there, we always fetch it *)
          if !queue <> Tools.IntMap.empty && fst (Tools.IntMap.min_binding !queue) = !next then
            incr available;
          while !available > 0 && !queue <> Tools.IntMap.empty do
            let chunk_id, w_id = Tools.IntMap.min_binding !queue in
            (* Tell the worker to send data *)
            output_byte o_2_w.(w_id) 0;
            flush o_2_w.(w_id);
            assert (not (Tools.IntMap.mem chunk_id !buf));
            buf := Tools.IntMap.add chunk_id (input_value w_2_o.(w_id):'b) !buf;
            (* Tell the worker to send the next notification *)
            output_byte o_2_w.(w_id) 0;
            flush o_2_w.(w_id);
            queue := Tools.IntMap.remove chunk_id !queue;
            decr available
          done;
          (* Output at most as many chunks at the number of workers *)
          available := threads;
          while !available > 0 && !buf <> Tools.IntMap.empty do
            let chunk_id, data = Tools.IntMap.min_binding !buf in
            if chunk_id = !next then begin
              h data;
              buf := Tools.IntMap.remove chunk_id !buf;
              incr next;
              decr available
            end else
              available := 0 (* Force exit from the cycle *)
          done
        done;
        (* There might be chunks left in the buffer *)
        while !buf <> Tools.IntMap.empty do
          let chunk_id, data = Tools.IntMap.min_binding !buf in
          assert (chunk_id = !next);
          h data;
          buf := Tools.IntMap.remove chunk_id !buf;
          incr next
        done;
        (* Switch off all the workers *)
        for ii = 0 to red_threads do
          output_byte o_2_w.(ii) 0;
          flush o_2_w.(ii)
        done;
        close_pipes_out o_2_w_pipes;
        close_pipes_in w_2_o_pipes
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
        Tools.Printf.teprintf "0 lines read\n";
      process_stream_chunkwise ~buffered_chunks_per_thread:buffered_chunks_per_thread
        (fun () ->
          if not !eof_reached then begin
            let bytes = ref 0 and read_base = !read and buf = ref [] in
            begin try
              while !bytes < max_block_bytes do (* We read at least one line *)
                let line = input_line input in
                Tools.List.accum buf line;
                bytes := String.length line + !bytes;
                incr read
              done
            with End_of_file ->
              eof_reached := true
            end;
            if verbose then
              Tools.Printf.teprintf "%d %s read\n" !read (Tools.String.pluralize_int "line" !read);
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
            Tools.Printf.teprintf "%d more %s processed\n" lines (Tools.String.pluralize_int "line" lines);
          lines_base, lines, Buffer.contents processing_buffer)
        (fun (_, buf_len, buf) ->
          written := !written + buf_len;
          Tools.Printf.fprintf output "%s%!" buf;
          if verbose then
            Tools.Printf.teprintf "%d %s written\n" !written (Tools.String.pluralize_int "line" !written))
        threads;
      flush output;
      if verbose then
        Tools.Printf.teprintf "%d %s out\n" !written (Tools.String.pluralize_int "line" !written)
  end

