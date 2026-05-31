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
  | Of_newick of string
  | Add_binary of string
  | Add_file of string
  | Add_newick of string * float
  | Drop_weak_splits of float
  | To_tree of string
  | To_binary of string
  | Set_precision of int
  | To_file of string

module Defaults =
  struct
    let precision = 10
    (*let threads = Processes.Parallel.get_nproc ()*)
    let verbose = false
    let negative_branches = Newick.NegativeBranchesPolicy.OK
  end

module Parameters =
  struct
    let program = ref []
    (*let threads = ref Defaults.threads*)
    let verbose = ref Defaults.verbose
    let negative_branches = ref Defaults.negative_branches
  end

let info = {
  Tools.Argv.name = "Yggdrasill";
  version = "5";
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
    [ "--input-tree"; "--Of-tree" ],
      Some "<newick_file>",
      [ "load into the splits register the bipartitions of the specified Newick tree";
        " (replaces the current register; for multi-tree files, every tree's";
        "  bipartitions are added, each with weight 1.0 per occurrence)" ],
      TA.Optional,
      (fun _ -> Of_newick (TA.get_parameter ()) |> List.accum Parameters.program);
    [ "--Add-tree" ],
      Some "<newick_file>",
      [ "add to the splits register the bipartitions of the specified Newick";
        "tree, each with weight 1.0 (so a bipartition supported by k trees";
        "accumulates to weight k).  Compose with multiple --Add-tree calls";
        "for ensemble consensus, then feed to -t" ],
      TA.Optional,
      (fun _ -> Add_newick (TA.get_parameter (), 1.0) |> List.accum Parameters.program);
    [ "--Add-tree-weighted" ],
      Some "<newick_file> <weight>",
      [ "as --Add-tree, but each bipartition gets the explicit weight given";
        " (useful when different ensemble members should count differently)" ],
      TA.Optional,
      (fun _ ->
        let file = TA.get_parameter () in
        let w = TA.get_parameter_float () in
        Add_newick (file, w) |> List.accum Parameters.program);
    [ "--drop-weak-splits" ],
      Some "<float>",
      [ "drop every split in the register whose accumulated weight is";
        "strictly less than the cutoff.  Apply between --Add-tree calls and";
        "-t to implement majority-rule consensus at threshold p:";
        " set cutoff = p * n_input_trees" ],
      TA.Optional,
      (fun _ -> Drop_weak_splits (TA.get_parameter_float_pos ())
                |> List.accum Parameters.program);
    [ "--negative-branches-policy" ],
      Some "'error'|'ok'|'zero'",
      [ "policy for handling negative branch lengths when reading Newick input";
        " (via --input-tree, --Add-tree, --Add-tree-weighted).";
        " 'error' rejects (raises a parse error); 'ok' accepts as-is;";
        " 'zero' silently clamps to 0.  Set before the --Add-tree call(s)";
        " it should apply to" ],
      TA.Default (Newick.NegativeBranchesPolicy.to_string Defaults.negative_branches |> Fun.const),
      (fun _ ->
        Parameters.negative_branches :=
          TA.get_parameter () |> Newick.NegativeBranchesPolicy.of_string);
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
      TA.Default (string_of_int Defaults.precision |> Fun.const),
      (fun _ -> Set_precision (TA.get_parameter_int_pos ()) |> List.accum Parameters.program);
    [ "-O"; "--Output" ],
      Some "<splits_file_prefix>",
      [ "dump the contents of the splits register to the specified plain text file";
        " (which will be given extension '.PhyloSplits.txt' unless file is '/dev/*')" ],
      TA.Optional,
      (fun _ -> To_file (TA.get_parameter ()) |> List.accum Parameters.program);
    TA.make_separator_multiline [ "Miscellaneous options."; "They are set immediately." ];
(*
    [ "-T"; "--threads" ],
      Some "<computing_threads>",
      [ "number of concurrent computing threads to be spawned";
        " (default automatically detected from your configuration)" ],
      TA.Default (string_of_int Defaults.threads |> Fun.const),
      (fun _ -> Parameters.threads := TA.get_parameter_int_pos ());
*)
    [ "-v"; "--verbose" ],
      None,
      [ "set verbose execution (global option)" ],
      TA.Default (Fun.const "quiet execution"),
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
        | Of_newick path ->
          splits :=
            Splits.of_newick_file
              ~negative_branches:!Parameters.negative_branches path
        | Add_newick (path, weight) ->
          (* First call (empty register) initialises; subsequent calls
             add to the existing register, accumulating weights.  The
             negative-branches policy is read from Parameters at the
             time the action runs, so a --negative-branches set later
             in the CLI still applies before its first --Add-tree. *)
          let neg = !Parameters.negative_branches in
          if Array.length (Splits.get_names !splits) = 0 then
            splits :=
              Splits.of_newick_file
                ~negative_branches:neg
                ~weight_kind:(Splits.Constant weight) path
          else
            Splits.add_newick_file
              ~negative_branches:neg
              ~weight_kind:(Splits.Constant weight) !splits path
        | Drop_weak_splits w ->
          Splits.drop_weight_below !splits w
        | To_tree prefix ->
          let filename_tree = prefix ^ ".nwk" in
          let splits_ok, tree, splits_ko = Trees.of_splits ~verbose:!Parameters.verbose !splits in
          Newick.to_file tree filename_tree;
          Splits.to_binary splits_ok prefix;
          (* Dropped-weight diagnostic: report how tree-like the input split set was.
             W_kept = sum of weights of accepted (compatible) splits; W_dropped = sum
             of weights of rejected (incompatible) splits; the ratio
             W_dropped / (W_kept + W_dropped) is a direct measure of non-tree-likeness. *)
          let w_kept = ref 0. and w_dropped = ref 0. in
          Splits.iter (fun _ w -> w_kept := !w_kept +. w) splits_ok;
          Splits.iter (fun _ w -> w_dropped := !w_dropped +. w) splits_ko;
          let total = !w_kept +. !w_dropped in
          Printf.eprintf
            "(%s): Tree built from %d compatible splits (total weight %.6g); %d incompatible splits dropped (total weight %.6g%s).\n%!"
            __FUNCTION__
            (Splits.cardinal splits_ok) !w_kept
            (Splits.cardinal splits_ko) !w_dropped
            (if total > 0. then
              Printf.sprintf "; W_dropped/(W_kept+W_dropped) = %.4f" (!w_dropped /. total)
            else
              "");
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

