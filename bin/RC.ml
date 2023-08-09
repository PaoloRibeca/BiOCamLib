(*
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
    let no_complement = ref false
  end

let version = "0.2"

let header =
  Printf.sprintf begin
    "This is the RC program (version %s)\n%!" ^^
    " (c) 2023 Paolo Ribeca, <paolo.ribeca@gmail.com>\n%!"
  end version

let () =
  let module TA = Tools.Argv in
  TA.set_header header;
  TA.set_synopsis "[OPTIONS]";
  TA.parse [
    TA.make_separator "Algorithm";
    [ "-C"; "--no-complement" ],
      None,
      [ "do not base-complement the sequence" ],
      TA.Default (fun () -> "base-complement"),
      (fun _ -> Parameters.no_complement := true);
    TA.make_separator "Miscellaneous";
    [ "-V"; "--version" ],
      None,
      [ "print version and exit" ],
      TA.Optional,
      (fun _ -> Printf.printf "%s\n%!" version; exit 0);
    (* Hidden option to emit help in markdown format *)
    [ "--markdown" ], None, [], TA.Optional, (fun _ -> TA.markdown (); exit 0);
    [ "-h"; "--help" ],
      None,
      [ "print syntax and exit" ],
      TA.Optional,
      (fun _ -> TA.usage (); exit 1)
  ];
  let f =
    if !Parameters.no_complement then
      Tools.String.rev
    else
      Sequences.Lint.rc in
  try
    while true do
      input_line stdin |> f |> Printf.printf "%s\n";
      flush stdout
    done
  with End_of_file -> ()

