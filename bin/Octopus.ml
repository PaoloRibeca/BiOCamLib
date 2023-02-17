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

module Defaults =
  struct
    let verbose = false
  end

module Parameters =
  struct
    let [@warning "-32"] verbose = ref Defaults.verbose
  end

let version = "0.3"

let header =
  Printf.sprintf begin
    "This is the Octopus program (version %s)\n%!" ^^
    " (c) 2016-2023 Paolo Ribeca, <paolo.ribeca@gmail.com>\n%!"
  end version

let () =
  let module TA = Tools.Argv in
  TA.set_header header;
  TA.set_synopsis "[OPTIONS]";
  TA.parse [
    TA.make_separator "Miscellaneous";
    (*
    [ "-t"; "--threads" ],
      Some "<positive_integer>",
      [ "number of concurrent computing threads to be spawned" ],
      TA.Default (fun () -> string_of_int !Parameters.threads),
      (fun _ -> Parameters.threads := TA.get_parameter_int_pos ());
    [ "-v"; "--verbose" ],
      None,
      [ "set verbose execution" ],
      TA.Default (fun () -> string_of_bool !Parameters.verbose),
      (fun _ -> Parameters.verbose := true);
    *)
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
  let module TC = Tools.StringTransitiveClosure in
  let re_whitespace = Str.regexp "[ \t]+" and res = TC.empty () |> ref in
  try
    while true do
      (* Note that whitespace at the beginning or the end of the line will be ignored *)
      let line = input_line stdin |> Tools.Split.as_list re_whitespace in
      match line with
      | [] -> ()
      | el :: [] ->
        res := TC.add_one !res el
      | hd :: tl ->
        List.iter
          (fun el ->
            res := TC.add_two !res hd el)
          tl
    done
  with End_of_file ->
    let done_first_line = ref false in
    TC.iter
      (fun () ->
        if !done_first_line then
          Printf.printf "\n%!"
        else
          done_first_line := true;
        let done_first_elem = ref false in
        (fun el ->
          if !done_first_elem then
            Printf.printf "\t"
          else
            done_first_elem := true;
          Printf.printf "%s" el))
      !res;
    if !done_first_line then
      Printf.printf "\n%!"

