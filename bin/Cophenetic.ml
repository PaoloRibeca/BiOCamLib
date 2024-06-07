(*
    Cophenetic.ml -- (c) 2023-2024 Paolo Ribeca, <paolo.ribeca@gmail.com>

    Cophenetic outputs to standard output the matrix of (cophenetic) distances
    between (internal and external) nodes of the Newick tree
    it receives on standard input.

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

module Parameters =
  struct
    let max_distances = ref false (* One usually wants to compute minimum distances *)
    let threads = Processes.Parallel.get_nproc () |> ref
    let verbose = ref false
  end

let info = {
  Tools.Argv.name = "Cophenetic";
  version = "2";
  date = "06-Jun-2024"
} and authors = [
  "2024", "Paolo Ribeca", "paolo.ribeca@gmail.com"
]

let () =
  let module TA = Tools.Argv in
  TA.set_header (info, authors, [ Info.info ]);
  TA.set_synopsis "[OPTIONS]";
  TA.parse [
    TA.make_separator "Algorithm";
    [ "-M"; "--max-distances"; "--maximum-distances"; "--longest-path-distances" ],
      None,
      [ "compute longest- rather than shortest-path distances" ],
      TA.Default (fun () -> "compute shortest-path distances"),
      (fun _ -> Parameters.max_distances := true);
    TA.make_separator "Miscellaneous";
    [ "-t"; "-T"; "--threads" ],
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
  let t = Trees.Newick.of_file "/dev/stdin" in
  let m = begin
    if !Parameters.max_distances then
      Trees.Newick.get_max_distance_matrix
    else
      Trees.Newick.get_min_distance_matrix
  end ~threads:!Parameters.threads ~verbose:!Parameters.verbose t in
  Matrix.to_file ~threads:!Parameters.threads ~verbose:!Parameters.verbose m "/dev/stdout"

