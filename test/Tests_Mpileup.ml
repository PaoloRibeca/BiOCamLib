(*
    Tests_Mpileup.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Tests_Mpileup.ml exercises the pileup reader: what each
    character of the read-bases column means, how the qualities column
    is counted out against it, and what the reader refuses.  Lines are
    written inline, small enough that the expected reading can be
    worked out from the format rather than from a run.

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

module M = Mpileup

(* Helpers. *)

let line columns = String.concat "\t" columns

(* One read rendered as call/strand/quality, plus whatever else it carries, so
   that a failing check shows the whole reading at once. *)
let show_read (r: M.Read.t) =
  Printf.sprintf "%s%s%d%s%s%s"
    (M.Call.to_string r.M.Read.call)
    (match r.M.Read.strand with
     | Sequences.Types.Forward _ -> "+"
     | Sequences.Types.Reverse _ -> "-")
    r.M.Read.quality
    (match r.M.Read.indel with None -> "" | Some i -> M.Indel.to_string i)
    (match r.M.Read.starts_read with None -> "" | Some q -> Printf.sprintf "^%d" q)
    (if r.M.Read.ends_read then "$" else "")

let show t =
  Printf.sprintf "%s:%d:%c:%d %s" t.M.seq t.M.pos t.M.reference t.M.depth
    (Array.to_list t.M.reads |> List.map show_read |> String.concat " ")

let read ?quality_offset s = M.of_line ?quality_offset s

(* The columns either side of the calls. *)

let test_columns () =
  Testing.section "Pileup columns" (fun () ->
    Testing.check_string "the five plain columns are read as they stand"
      ~expected:"chr1:100:A:3 .+40 .-40 .+40"
      (show (read (line [ "chr1"; "100"; "A"; "3"; ".,."; "III" ])));
    (* 'I' is 73, and 73 - 33 is 40, which is what a decent base looks like *)
    Testing.check_string "a quality is decoded against the offset"
      ~expected:"chr1:100:A:1 .+40"
      (show (read (line [ "chr1"; "100"; "A"; "1"; "."; "I" ])));
    Testing.check_string "and against another offset if the caller says so"
      ~expected:"chr1:100:A:1 .+9"
      (show (read ~quality_offset:64 (line [ "chr1"; "100"; "A"; "1"; "."; "I" ])));
    (* A seventh column of mapping qualities is what 'samtools mpileup -s'
       writes; the reader takes the six it knows and leaves the rest alone. *)
    Testing.check_string "a seventh column is not in the way"
      ~expected:"chr1:100:A:1 .+40"
      (show (read (line [ "chr1"; "100"; "A"; "1"; "."; "I"; "]" ])));
    (* Depth zero writes an asterisk in both columns, and that asterisk is a
       placeholder rather than a deleted base: reading it as one would invent a
       read where the file says there are none. *)
    Testing.check_string "a line at depth zero holds no reads"
      ~expected:"chr1:100:A:0 "
      (show (read (line [ "chr1"; "100"; "A"; "0"; "*"; "*" ]))))

(* What each character of the read-bases column means. *)

let test_calls () =
  Testing.section "Pileup calls" (fun () ->
    let calls s quals =
      show (read (line [ "c"; "1"; "A"; string_of_int (String.length quals); s; quals ])) in
    Testing.check_string "a dot and a comma are the reference, on either strand"
      ~expected:"c:1:A:2 .+40 .-40" (calls ".," "II");
    Testing.check_string "a letter is a base, and its case is the strand"
      ~expected:"c:1:A:2 G+40 G-40" (calls "Gg" "II");
    Testing.check_string "an asterisk and a hash are a deleted base"
      ~expected:"c:1:A:2 *+40 *-40" (calls "*#" "II");
    Testing.check_string "an angle bracket is the read skipping the reference"
      ~expected:"c:1:A:2 >+40 >-40" (calls "><" "II");
    (* The case of an indel's bases is the strand of the read it hangs off,
       which says nothing about the bases, so it is normalised away. *)
    Testing.check_string "an insertion hangs off the base before it"
      ~expected:"c:1:A:1 .+40+AC" (calls ".+2AC" "I");
    Testing.check_string "and a deletion likewise"
      ~expected:"c:1:A:1 .+40-G" (calls ".-1g" "I");
    Testing.check_string "an indel of more than nine bases reads its whole length"
      ~expected:"c:1:A:1 .+40+ACGTACGTAC" (calls ".+10ACGTACGTAC" "I");
    Testing.check_string "a caret introduces a read and carries its mapping quality"
      ~expected:"c:1:A:1 .+40^42" (calls "^K." "I");
    Testing.check_string "a dollar ends one"
      ~expected:"c:1:A:1 .+40$" (calls ".$" "I");
    (* Everything at once, in the order the format writes it. *)
    Testing.check_string "and a read may begin, carry an indel and end at once"
      ~expected:"c:1:A:1 .+40+AC^42$" (calls "^K.+2AC$" "I");
    (* A caret's quality is an arbitrary character, including one the reader
       would otherwise have taken for a call. *)
    Testing.check_string "a caret's quality is never read as a call"
      ~expected:"c:1:A:2 .+40^13 .+40" (calls "^..." "II"))

(* Writing one back out, which is what says the reading kept everything. *)

let test_round_trip () =
  Testing.section "Pileup round trip" (fun () ->
    List.iter
      (fun l ->
        Testing.check_string (Printf.sprintf "%S survives being read and written" l)
          ~expected:l (M.to_string (M.of_line l)))
      [ line [ "chr1"; "100"; "A"; "3"; ".,."; "III" ];
        line [ "chr1"; "100"; "A"; "2"; "Gg"; "IJ" ];
        line [ "chr1"; "100"; "A"; "4"; "*#><"; "IIII" ];
        line [ "chr1"; "100"; "A"; "1"; "^K.+2AC$"; "I" ];
        line [ "chr1"; "100"; "A"; "2"; ".-1G,"; "II" ];
        line [ "chr1"; "100"; "A"; "0"; "*"; "*" ] ])

(* What it refuses.  A pileup is machine-written, so anything unexpected in one
   means a tool this reader has not met or a truncated file, and saying which
   line and where in the column is most of the diagnosis. *)

let test_refusals () =
  Testing.section "Pileup refusals" (fun () ->
    Testing.check_raises ~re:"at least 6 columns" "a short line is refused"
      (fun () -> ignore (read (line [ "c"; "1"; "A"; "1"; "." ])));
    Testing.check_raises ~re:"Invalid position" "so is a position that is not one"
      (fun () -> ignore (read (line [ "c"; "x"; "A"; "1"; "."; "I" ])));
    Testing.check_raises ~re:"Invalid depth" "and a depth that is not one"
      (fun () -> ignore (read (line [ "c"; "1"; "A"; "x"; "."; "I" ])));
    Testing.check_raises ~re:"Invalid reference" "and a reference of more than one base"
      (fun () -> ignore (read (line [ "c"; "1"; "AC"; "1"; "."; "I" ])));
    Testing.check_raises ~re:"disagree" "columns of different lengths are refused"
      (fun () -> ignore (read (line [ "c"; "1"; "A"; "2"; ".."; "I" ])));
    Testing.check_raises ~re:"Depth column says" "as is a depth that counted differently"
      (fun () -> ignore (read (line [ "c"; "1"; "A"; "5"; ".."; "II" ])));
    Testing.check_raises ~re:"Unexpected character" "an unknown character is refused"
      (fun () -> ignore (read (line [ "c"; "1"; "A"; "1"; "?"; "I" ])));
    Testing.check_raises ~re:"past the end" "and an indel longer than what follows it"
      (fun () -> ignore (read (line [ "c"; "1"; "A"; "1"; ".+9AC"; "I" ])));
    Testing.check_raises ~re:"offset" "the message says where in the column"
      (fun () -> ignore (read (line [ "c"; "1"; "A"; "3"; "..?"; "III" ])));
    Testing.check_raises ~re:"On line 2" "and which line of the input"
      (fun () ->
        M.iter_string (fun _ -> ())
          (line [ "c"; "1"; "A"; "1"; "."; "I" ] ^ "\n"
           ^ line [ "c"; "2"; "A"; "1"; "?"; "I" ] ^ "\n")))

(* Reading many lines, from a string and from a file. *)

let test_iteration () =
  Testing.section "Pileup iteration" (fun () ->
    let text =
      line [ "c"; "1"; "A"; "1"; "."; "I" ] ^ "\n"
      ^ line [ "c"; "2"; "C"; "2"; ".,"; "II" ] ^ "\n"
      ^ line [ "c"; "3"; "G"; "0"; "*"; "*" ] ^ "\n" in
    let collect f =
      let acc = ref [] in
      f (fun t -> List.accum acc (Printf.sprintf "%d:%d" t.M.pos t.M.depth));
      List.rev !acc |> String.concat " " in
    Testing.check_string "every line of a string is read, in order"
      ~expected:"1:1 2:2 3:0" (collect (fun f -> M.iter_string f text));
    let path = Filename.temp_file "BiOCamLib_Tests_" ".pileup" in
    Fun.protect ~finally:(fun () -> Sys.remove path)
      (fun () ->
        let oc = open_out path in
        output_string oc text;
        close_out oc;
        Testing.check_string "and every line of a file, the same way"
          ~expected:"1:1 2:2 3:0" (collect (fun f -> M.iter f path)));
    Testing.check_raises ~re:"Input file not found" "a missing file is refused as such"
      (fun () -> M.iter (fun _ -> ()) "/nonexistent/BiOCamLib_Tests_missing.pileup"))

let run () =
  test_columns ();
  test_calls ();
  test_round_trip ();
  test_refusals ();
  test_iteration ()
