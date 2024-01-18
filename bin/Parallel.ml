(*
    Parallel.ml -- (c) 2019-2024 Paolo Ribeca, <paolo.ribeca@gmail.com>

    Parallel allows to split and process an input file chunk-wise,
    using the reader/workers/writer model implemented in
    BiOCamLib.Tools.Parallel.

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

module Parameters =
  struct
    let command = ref ""
    let args = ref [||]
    let lines_per_block = ref 10000
    let input = ref ""
    let output = ref ""
    let threads = Processes.Parallel.get_nproc () |> ref
    let verbose = ref false
    let debug = ref false
  end

let info = {
  Tools.Argv.name = "Parallel";
  version = "8";
  date = "18-Jan-2024"
} and authors = [
  "2019-2024", "Paolo Ribeca", "paolo.ribeca@gmail.com"
]

let () =
  let module TA = Tools.Argv in
  TA.set_header (info, authors, [ Info.info ]);
  TA.set_synopsis "[OPTIONS] -- [COMMAND TO PARALLELIZE AND ITS OPTIONS]";
  TA.parse [
    TA.make_separator "Command to parallelize";
    [ "--" ],
      None,
      [ "consider all the subsequent parameters";
        "as the command to be executed in parallel.";
        "At least one command must be specified" ],
      TA.Mandatory,
      (fun _ ->
        Parameters.command := TA.get_parameter ();
        Parameters.args := Array.append [| !Parameters.command |] (TA.get_remaining_parameters ()));
    TA.make_separator "Input/Output";
    [ "-l"; "--lines-per-block" ],
      Some "<positive_integer>",
      [ "number of lines to be processed per block" ],
      TA.Default (fun () -> string_of_int !Parameters.lines_per_block),
      (fun _ -> Parameters.lines_per_block := TA.get_parameter_int_pos ());
    [ "-i"; "--input" ],
      Some "<input_file>",
      [ "name of input file" ],
      TA.Default (fun () -> "stdin"),
      (fun _ -> Parameters.input := TA.get_parameter ());
    [ "-o"; "--output" ],
      Some "<output_file>",
      [ "name of output file" ],
      TA.Default (fun () -> "stdout"),
      (fun _ -> Parameters.output := TA.get_parameter ());
    TA.make_separator "Miscellaneous";
    [ "-t"; "--threads" ],
      Some "<positive_integer>",
      [ "number of concurrent computing threads to be spawned";
        " (default automatically detected from your configuration)" ],
      TA.Default (fun () -> string_of_int !Parameters.threads),
      (fun _ -> Parameters.threads := TA.get_parameter_int_pos ());
    [ "-v"; "--verbose" ],
      None,
      [ "set verbose execution" ],
      TA.Default (fun () -> string_of_bool !Parameters.verbose),
      (fun _ -> Parameters.verbose := true);
    [ "-d"; "--debug" ],
      None,
      [ "output debugging information" ],
      TA.Default (fun () -> string_of_bool !Parameters.debug),
      (fun _ -> Parameters.debug := true);
    [ "-V"; "--version" ],
      None,
      [ "print version and exit" ],
      TA.Optional,
      (fun _ -> Printf.printf "%s\n%!" info.version; exit 0);
    (* Hidden option to emit help in markdown format *)
    [ "--markdown" ], None, [], TA.Optional, (fun _ -> TA.markdown (); exit 0);
    [ "-h"; "--help" ],
      None,
      [ "print syntax and exit" ],
      TA.Optional,
      (fun _ -> TA.usage (); exit 1)
  ];
  let verbose = !Parameters.verbose and debug = !Parameters.debug in
  if verbose then
    TA.header ();
  try
    let input =
      if !Parameters.input = "" then
        stdin
      else
        open_in !Parameters.input
    and output =
      if !Parameters.output = "" then
        stdout
      else
        open_out !Parameters.output in
    let input_line_num = ref 0 in
    let print_num_lines what =
      if verbose then
        Printf.sprintf "%d %s %s" !input_line_num (Tools.String.pluralize_int "line" !input_line_num) what |>
        Tools.Printf.pteprintf "%s\n%!"
    and wait_and_check pid =
      match let _, status = Unix.waitpid [] pid in status with
      | Unix.WEXITED 0 -> ()
      | _ ->
        Printf.eprintf "%s: %s\n%!"
          (Printf.sprintf "On line %d" !input_line_num |> Tools.String.TermIO.blue)
          (Printf.sprintf "FATAL: subprocess [#%07d->#%07d] terminated with an error" (Unix.getpid ()) pid
            |> Tools.String.TermIO.red);
        exit 1 in
    let lines_per_block = !Parameters.lines_per_block
    and eof = ref false and processing_buffer = Buffer.create 16777216 in
    Processes.Parallel.process_stream_chunkwise
      (fun () ->
        if not !eof then begin
          if !input_line_num mod lines_per_block = 0 then
            print_num_lines "read";
          let buf = ref [] and base_input_line_num = !input_line_num + 1 in
          begin try
            for _ = 1 to lines_per_block do
              incr input_line_num;
              input_line input |> Tools.List.accum buf
            done
          with End_of_file ->
            decr input_line_num;
            print_num_lines "read";
            eof := true
          end;
          base_input_line_num, !buf
        end else
          raise End_of_file)
      (fun (base_input_line_num, input_lines) ->
        (* We want to keep counters realiable in case of error *)
        input_line_num := base_input_line_num;
        let input_lines_processed = ref 0 in
        Buffer.clear processing_buffer;
        (* We start the subprocess *)
        let in_pipe_out, in_pipe_in = Unix.pipe ()
        and out_pipe_out, out_pipe_in = Unix.pipe () in
        match Unix.fork () with
        | 0 -> (* Child *)
          Unix.close in_pipe_in;
          Unix.close out_pipe_out;
          (* We connect the first pipe to stdin and the second pipe to stdout *)
          Unix.dup2 in_pipe_out Unix.stdin;
          Unix.dup2 out_pipe_in Unix.stdout;
          Unix.close in_pipe_out;
          Unix.close out_pipe_in;
          Unix.unsafe_environment () |> Unix.execvpe !Parameters.command !Parameters.args
          (* We will never get here *)
        | pid_child -> (* Parent *)
          Unix.close in_pipe_out;
          Unix.close out_pipe_in;
          (* We communicate with the subprocess through the pipes *)
          let process_input = Unix.out_channel_of_descr in_pipe_in
          and process_output = Unix.in_channel_of_descr out_pipe_out in
  (* Is this really necessary?
          set_binary_mode_in process_output false;
          set_binary_mode_out process_input false;
  *)
          match Unix.fork () with
          | 0 -> (* Grandchild *)
            close_in process_output;
            (* We feed the input lines to the subprocess *)
            List.iter
              (fun line ->
                if debug then
                  Tools.Printf.peprintf ">>> Pushing line '%s'\n%!" line;
                output_string process_input line;
                output_char process_input '\n')
              (List.rev input_lines);
              flush process_input;
            close_out process_input;
    (*        Unix.close in_pipe_in; *)
            if debug then
              Tools.Printf.peprintf ">>> Pushing done.\n%!";
            (* Commit suicide *)
            Unix._exit 0 (* Do not flush buffers or do anything else *)
          | pid_grandchild -> (* Grandparent *)
            close_out process_input;
            (* We have to update the counter here, otherwise the information will be lost *)
            input_lines_processed := !input_lines_processed + List.length input_lines;
            (* We read the output from the subprocess, and accumulate it into the buffer.
              Note that in principle the number of lines in output
              could differ from the number of lines in input *)
            begin try
              while true do
                let line = input_line process_output in
                if debug then
                  Tools.Printf.peprintf "<<< Pulling line '%s'\n%!" line;
                Buffer.add_string processing_buffer line;
                Buffer.add_char processing_buffer '\n'
              done
            with End_of_file -> ()
            end;
            if debug then
              Tools.Printf.peprintf "<<< Pulling done.\n%!";
    (*        Unix.close out_pipe_out; *)
            close_in process_output;
            (* We make sure that the children are not zombying around *)
            wait_and_check pid_child;
            wait_and_check pid_grandchild;
            !input_lines_processed, Buffer.contents processing_buffer)
      (fun (processed, buf) ->
        if !input_line_num = 0 then
          print_num_lines "processed";
        let old_periods = !input_line_num / lines_per_block in
        input_line_num := !input_line_num + processed;
        if !input_line_num / lines_per_block > old_periods then
          print_num_lines "processed";
        Printf.fprintf output "%s%!" buf)
      !Parameters.threads;
    print_num_lines "processed";
    (* Cleanup actions *)
    close_in input;
    close_out output
  with exc ->
    Tools.Printf.peprintf "(%s): %s\n%!" __FUNCTION__
      ("FATAL: Uncaught exception: " ^ Printexc.to_string exc |> Tools.String.TermIO.red)

