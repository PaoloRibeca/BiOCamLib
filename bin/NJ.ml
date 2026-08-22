(*
    NJ.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    NJ builds a neighbour-joining tree from a matrix of pairwise distances
    and writes it out in Newick format.  The tree it produces is unrooted,
    as neighbour joining's is; option -m re-roots it at the midpoint of its
    longest tip-to-tip path, which is what one does when no outgroup is at
    hand.  It is the counterpart of Cophenetic, which goes the other way.

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

module Defaults =
  struct
    let input = "/dev/stdin"
    let output = "/dev/stdout"
    let asymmetry = Trees.NeighbourJoining.AsymmetryPolicy.Average
    let negative_branches = Trees.Newick.NegativeBranchesPolicy.OK
    let midpoint = false
    let rich_format = false
    let threads = Processes.Parallel.get_nproc ()
    let verbose = false
  end

module Parameters =
  struct
    let input = ref Defaults.input
    let output = ref Defaults.output
    let asymmetry = ref Defaults.asymmetry
    let negative_branches = ref Defaults.negative_branches
    let midpoint = ref Defaults.midpoint
    let rich_format = ref Defaults.rich_format
    let threads = ref Defaults.threads
    let verbose = ref Defaults.verbose
  end

let info = {
  Tools.Argv.name = "NJ";
  version = "1";
  date = "22-Aug-2026"
} and authors = [
  "2026", "Paolo Ribeca", "paolo.ribeca@gmail.com"
]

let () =
  let module TA = Tools.Argv in
  try
    TA.set_header (info, authors, [ Info.info ]);
    TA.set_synopsis "[OPTIONS]";
    TA.parse [
      TA.make_separator_multiline
        [ "Input/Output.";
          "The distance matrix is the tab-separated form the rest of this suite reads and";
          " writes: a header line of column names preceded by an empty field, and one row";
          " per name.  Both axes must carry the same names in the same order" ];
      [ "-i"; "--input" ],
        Some "<distance_matrix>",
        [ "name of the file containing the matrix of pairwise distances" ],
        TA.Default (Fun.const Defaults.input),
        (fun _ -> Parameters.input := TA.get_parameter ());
      [ "-o"; "--output" ],
        Some "<newick_file>",
        [ "name of the file the resulting tree should be written to" ],
        TA.Default (Fun.const Defaults.output),
        (fun _ -> Parameters.output := TA.get_parameter ());
      [ "-r"; "--rich-format" ],
        None,
        [ "emit the rich Newick dialect this suite understands, which tags the";
          " tree as rooted or unrooted and carries dictionaries and hybrid";
          " nodes; plain Newick is what most other programs expect" ],
        TA.Default (string_of_bool Defaults.rich_format |> Fun.const),
        (fun _ -> Parameters.rich_format := true);
      TA.make_separator "Algorithm";
      [ "-m"; "--midpoint"; "--midpoint-root" ],
        None,
        [ "re-root the tree at the midpoint of its longest tip-to-tip path,";
          " rather than leaving it unrooted as neighbour joining produces it" ],
        TA.Default (string_of_bool Defaults.midpoint |> Fun.const),
        (fun _ -> Parameters.midpoint := true);
      [ "-a"; "--asymmetry" ],
        Some "'average'|'error'",
        [ "what to do when the matrix disagrees with itself across the";
          " diagonal: replace both cells with their mean, or refuse the matrix" ],
        TA.Default (Trees.NeighbourJoining.AsymmetryPolicy.to_string Defaults.asymmetry |> Fun.const),
        (fun _ ->
          Parameters.asymmetry :=
            TA.get_parameter () |> Trees.NeighbourJoining.AsymmetryPolicy.of_string);
      [ "-n"; "--negative-branches" ],
        Some "'ok'|'zero'|'error'",
        [ "what to do about the branches of negative length a matrix that is";
          " not additive yields: keep them, flatten them to zero, or refuse";
          " the matrix" ],
        TA.Default (Trees.Newick.NegativeBranchesPolicy.to_string Defaults.negative_branches |> Fun.const),
        (fun _ ->
          Parameters.negative_branches :=
            TA.get_parameter () |> Trees.Newick.NegativeBranchesPolicy.of_string);
      TA.make_separator "Miscellaneous";
      [ "-t"; "-T"; "--threads" ],
        Some "<computing_threads>",
        [ "number of concurrent computing threads to be spawned";
          " (used when reading the matrix; the joining itself is sequential)";
          " (default automatically detected from your configuration)" ],
        TA.Default (string_of_int Defaults.threads |> Fun.const),
        (fun _ -> Parameters.threads := TA.get_parameter_int_pos ());
      [ "-v"; "--verbose" ],
        None,
        [ "set verbose execution (global option)" ],
        TA.Default (string_of_bool Defaults.verbose |> Fun.const),
        (fun _ -> Parameters.verbose := true);
      [ "-V"; "--version" ],
        None,
        [ "print version and exit" ],
        TA.Optional,
        (fun _ -> Printf.printf "%s\n%!" info.version; exit 0);
      (* Hidden option to emit help in markdown format *)
      [ "--markdown" ], None, [], TA.Optional, (fun _ -> TA.markdown (); exit 0);
      [ "-x"; "--print-exception-backtrace" ], None, [], TA.Optional,
        (fun _ -> Printexc.record_backtrace true);
      [ "-h"; "--help" ],
        None,
        [ "print syntax and exit" ],
        TA.Optional,
        (fun _ -> TA.usage (); exit 1)
    ];
    let m = Matrix.of_file ~threads:!Parameters.threads ~verbose:!Parameters.verbose !Parameters.input in
    let t =
      Trees.NeighbourJoining.of_matrix ~asymmetry:!Parameters.asymmetry
        ~negative_branches:!Parameters.negative_branches ~verbose:!Parameters.verbose m in
    let t =
      if !Parameters.midpoint then
        Trees.Newick.midpoint_root t
      else
        t in
    Trees.Newick.to_file ~rich_format:!Parameters.rich_format t !Parameters.output
  with e ->
    Exception.handle __FUNCTION__ TA.usage (fun () ->
      Printf.peprintf
        "(%s): This should not have happened - please contact <paolo.ribeca@gmail.com>\n%!"
        __FUNCTION__;
      Printf.peprintf
        "(%s): You might also wish to rerun me with option -x to get a full backtrace.\n%!"
        __FUNCTION__
    ) e

