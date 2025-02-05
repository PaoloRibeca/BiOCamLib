(*
    Yggdrasill.ml -- (c) 2024-2025 Paolo Ribeca, <paolo.ribeca@gmail.com>

    Yggdrasill supports several operations to build trees from splits.

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
open Trees

type to_do_t =
  | Clear
  | Of_binary of string
  | Of_file of string
  | Add_binary of string
  | Add_file of string
  | To_tree of string
  | To_binary of string
  | Set_precision of int
  | To_file of string

module Defaults =
  struct
    let precision = 15
  end

module Parameters =
  struct
    let program = ref []
    let threads = Processes.Parallel.get_nproc () |> ref
    let verbose = ref false
  end

let info = {
  Tools.Argv.name = "Yggdrasill";
  version = "3";
  date = "05-Feb-2025"
} and authors = [
  "2024-2025", "Paolo Ribeca", "paolo.ribeca@gmail.com"
]

let () =
  let module TA = Tools.Argv in
  TA.set_header (info, authors, [ Info.info ]);
  TA.set_synopsis "[OPTIONS]";
  TA.parse [
    TA.make_separator_multiline [ "Actions."; "They are executed delayed and in order of specification." ];
    [ "-c"; "--clear" ],
      None,
      [ "clear the splits register (keep names, discard existing splits)" ],
      TA.Optional,
      (fun _ -> Clear |> List.accum Parameters.program);
    [ "-i"; "--input" ],
      Some "<binary_file_prefix>",
      [ "load into the splits register the specified binary database";
        " (which must have extension '.PhyloSplits' unless file is '/dev/*')" ],
      TA.Optional,
      (fun _ -> Of_binary (TA.get_parameter ()) |> List.accum Parameters.program);
    [ "-I"; "--Input" ],
      Some "<splits_file_prefix>",
      [ "load into the splits register the specified plain text database";
        " (which must have extension '.PhyloSplits.txt' unless file is '/dev/*')" ],
      TA.Optional,
      (fun _ -> Of_file (TA.get_parameter ()) |> List.accum Parameters.program);
    [ "-a"; "--add" ],
      Some "<binary_file_prefix>",
      [ "add to the contents of the splits register the specified binary database";
        " (which must have extension '.PhyloSplits' unless file is '/dev/*')" ],
      TA.Optional,
      (fun _ -> Add_binary (TA.get_parameter ()) |> List.accum Parameters.program);
    [ "-A"; "--Add" ],
      Some "<splits_file_prefix>",
      [ "add to the contents of the splits register the specified plain text database";
        " (which must have extension '.PhyloSplits.txt' unless file is '/dev/*')" ],
      TA.Optional,
      (fun _ -> Add_file (TA.get_parameter ()) |> List.accum Parameters.program);
    [ "-t"; "--tree" ],
      Some "<tree_file_prefix>",
      [ "generate a phylogenetic tree from the contents of the splits register.";
        "The results will be a Newick file";
        " (which will be given extension '.nwk' unless file is '/dev/*')";
        "and the database of compatible splits used to build the tree";
        " (which will be given extension '.PhyloSplits.txt' unless file is '/dev/*').";
        "The residual incompatible splits will be moved back to the splits register" ],
      TA.Optional,
      (fun _ -> To_tree (TA.get_parameter ()) |> List.accum Parameters.program);
    [ "-o"; "--output" ],
      Some "<binary_file_prefix>",
      [ "dump the contents of the splits register to the specified binary file";
        " (which will be given extension '.PhyloSplits' unless file is '/dev/*')" ],
      TA.Optional,
      (fun _ -> To_binary (TA.get_parameter ()) |> List.accum Parameters.program);
    [ "--precision" ],
      Some "<positive_integer>",
      [ "set the number of precision digits to be used when outputting numbers" ],
      TA.Default (fun () -> string_of_int Defaults.precision),
      (fun _ -> Set_precision (TA.get_parameter_int_pos ()) |> List.accum Parameters.program);
    [ "-O"; "--Output" ],
      Some "<splits_file_prefix>",
      [ "dump the contents of the splits register to the specified plain text file";
        " (which will be given extension '.PhyloSplits.txt' unless file is '/dev/*')" ],
      TA.Optional,
      (fun _ -> To_file (TA.get_parameter ()) |> List.accum Parameters.program);
    TA.make_separator_multiline [ "Miscellaneous options."; "They are set immediately." ];
    [ "-T"; "--threads" ],
      Some "<computing_threads>",
      [ "number of concurrent computing threads to be spawned";
        " (default automatically detected from your configuration)" ],
      TA.Default (fun () -> string_of_int !Parameters.threads),
      (fun _ -> Parameters.threads := TA.get_parameter_int_pos ());
    [ "-v"; "--verbose" ],
      None,
      [ "set verbose execution (global option)" ],
      TA.Default (fun () -> string_of_bool !Parameters.verbose),
      (fun _ -> Parameters.verbose := true);
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
  let program = List.rev !Parameters.program in
  if program = [] then begin
    TA.usage ();
    exit 0
  end;
  if !Parameters.verbose then
    TA.header ();
  (* These are the registers available to the program *)
  let splits = Splits.create [| |] |> ref and precision = ref Defaults.precision in
  try
    List.iter
      (function
        | Clear ->
          Splits.clear !splits
        | Of_binary prefix ->
          splits := Splits.of_binary ~verbose:!Parameters.verbose prefix
        | Of_file prefix ->
          splits := Splits.of_file prefix
        | Add_binary prefix ->
          Splits.of_binary ~verbose:!Parameters.verbose prefix |> Splits.add_splits !splits
        | Add_file prefix ->
          Splits.of_file prefix |> Splits.add_splits !splits
        | To_tree prefix ->
          let filename_tree = prefix ^ ".nwk" in
          let splits_ok, tree, splits_ko = Trees.Splits.to_tree ~verbose:!Parameters.verbose !splits in
          Newick.to_file tree filename_tree;
          Splits.to_binary splits_ok prefix;
          splits := splits_ko
        | To_binary prefix ->
          Splits.to_binary ~verbose:!Parameters.verbose !splits prefix
        | Set_precision prec ->
          precision := prec
        | To_file prefix ->
          Splits.to_file ~precision:!precision !splits prefix)
      program
  with exc ->
    Printf.eprintf "[#%s]: (%s): %s\n%!" (Unix.getpid () |> string_of_int |> String.TermIO.blue) __FUNCTION__
      ("FATAL: Uncaught exception: " ^ Printexc.to_string exc |> String.TermIO.red)

