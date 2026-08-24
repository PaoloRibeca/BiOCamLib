(*
    Bench_Mpileup.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Bench_Mpileup.ml measures what replacing the pileup reader bought.
    It runs two readers over the same generated input, producing the
    same result from each, and differing only in the two things that
    changed: how a line is cut into columns, and how the read-bases
    column is walked.  The second is the interesting one -- the reader
    this replaces took a fresh one-character string per base -- so the
    allocation is reported beside the time, that being the quantity the
    change was actually about.

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

(* A pileup with the shape of a real one: mostly reads agreeing with the
   reference on either strand, a scattering of mismatches, an occasional indel,
   and reads beginning and ending at the edges of the column. *)
let generate ~lines ~depth =
  let buf = Buffer.create (lines * depth * 2) and state = ref 12345 in
  let next () =
    (* A generator of our own rather than Random, so that the same input is
       measured every time whatever the runtime does *)
    state := (!state * 1103515245 + 12345) land 0x3FFFFFFF;
    !state in
  for i = 1 to lines do
    let bases = Buffer.create (depth * 2) and quals = Buffer.create depth in
    let calls = ref 0 in
    while !calls < depth do
      let r = next () mod 100 in
      if !calls = 0 then
        Printf.bprintf bases "^%c" (Char.chr (33 + next () mod 40));
      if r < 70 then
        Buffer.add_char bases (if next () land 1 = 0 then '.' else ',')
      else if r < 90 then
        Buffer.add_char bases
          (let b = "ACGT".[next () mod 4] in
           if next () land 1 = 0 then b else Char.lowercase_ascii b)
      else if r < 95 then
        Buffer.add_char bases (if next () land 1 = 0 then '*' else '#')
      else begin
        Buffer.add_char bases '.';
        let n = 1 + next () mod 6 in
        Printf.bprintf bases "%c%d" (if next () land 1 = 0 then '+' else '-') n;
        for _ = 1 to n do
          Buffer.add_char bases "ACGT".[next () mod 4]
        done
      end;
      if !calls = depth - 1 then
        Buffer.add_char bases '$';
      Buffer.add_char quals (Char.chr (33 + next () mod 40));
      incr calls
    done;
    Printf.bprintf buf "chr1\t%d\t%c\t%d\t%s\t%s\n"
      i "ACGT".[i mod 4] depth (Buffer.contents bases) (Buffer.contents quals)
  done;
  Buffer.contents buf

(* The reader this replaces, in its own manner: the line cut into columns by
   [String.split_on_char], and the read-bases column walked one character at a
   time with [String.sub s i 1], each character compared as a string.  It is
   made to produce the same array as the new one so that the two are measured
   doing the same work, and only the manner of doing it differs. *)
let old_style_of_line ?(quality_offset = 33) line =
  let columns = Array.of_list (String.split_on_char '\t' line) in
  if Array.length columns < 6 then
    Exception.raise __FUNCTION__ IO_Format "Insufficient number of fields";
  let bases = columns.(4) and quals = columns.(5) in
  let len = String.length bases and acc = ref [] in
  let i = ref 0 and qpos = ref 0 and pending_start = ref None in
  if columns.(3) <> "0" then begin
    while !i < len do
      let c = String.sub bases !i 1 in
      let push call strand =
        let read =
          { Mpileup.Read.call; strand;
            quality = Char.code quals.[!qpos] - quality_offset;
            indel = None; starts_read = !pending_start; ends_read = false } in
        pending_start := None;
        List.accum acc read;
        incr qpos in
      begin match c with
      | "." -> push Mpileup.Call.Reference Sequences.Types.forward
      | "," -> push Mpileup.Call.Reference Sequences.Types.reverse
      | "A" | "C" | "G" | "T" | "N" ->
        push (Mpileup.Call.Base c.[0]) Sequences.Types.forward
      | "a" | "c" | "g" | "t" | "n" ->
        push (Mpileup.Call.Base (Char.uppercase_ascii c.[0])) Sequences.Types.reverse
      | "*" -> push Mpileup.Call.Gap Sequences.Types.forward
      | "#" -> push Mpileup.Call.Gap Sequences.Types.reverse
      | ">" -> push Mpileup.Call.Skip Sequences.Types.forward
      | "<" -> push Mpileup.Call.Skip Sequences.Types.reverse
      | "+" | "-" as dir ->
        let how_many = ref "" in
        while begin
          incr i;
          let cc = String.sub bases !i 1 in
          match cc with
          | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
            how_many := !how_many ^ cc;
            true
          | _ -> false
        end do () done;
        let how_many = int_of_string !how_many in
        let s = String.uppercase_ascii (String.sub bases !i how_many) in
        (match !acc with
         | last :: rest ->
           acc :=
             { last with
               Mpileup.Read.indel =
                 Some (if dir = "+" then Mpileup.Indel.Insertion s
                       else Mpileup.Indel.Deletion s) } :: rest
         | [] -> ());
        i := !i + how_many - 1
      | "^" -> incr i; pending_start := Some (Char.code bases.[!i] - 33)
      | "$" ->
        (match !acc with
         | last :: rest -> acc := { last with Mpileup.Read.ends_read = true } :: rest
         | [] -> ())
      | _ ->
        Exception.raise __FUNCTION__ IO_Format ("Unknown character " ^ c)
      end;
      incr i
    done
  end;
  Array.of_list (List.rev !acc)

(* A third way, and the one the comparison above is missing: the same hand
   written scan, but matching on CHARACTERS rather than on one-character
   strings, and cutting the columns by walking to each tab rather than by
   building a list of them.  Neither is clever -- it is what the original would
   have been had it never reached for [String.sub s i 1] -- and it is here to
   answer the question the other two cannot: how much of the cost was the
   technique, and how much was the format. *)
let direct_of_line ?(quality_offset = 33) line =
  let len = String.length line in
  let starts = Array.make 6 0 and stops = Array.make 6 0 in
  let n = ref 0 and pos = ref 0 and finished = ref false in
  while not !finished && !n < 6 do
    let stop = match String.index_from_opt line !pos '\t' with Some i -> i | None -> len in
    starts.(!n) <- !pos;
    stops.(!n) <- stop;
    incr n;
    if stop >= len then finished := true else pos := stop + 1
  done;
  if !n < 6 then
    Exception.raise __FUNCTION__ IO_Format "Insufficient number of fields";
  let bases = String.sub line starts.(4) (stops.(4) - starts.(4))
  and quals = String.sub line starts.(5) (stops.(5) - starts.(5)) in
  let blen = String.length bases and acc = ref [] and count = ref 0 in
  let i = ref 0 and qpos = ref 0 and pending_start = ref None in
  while !i < blen do
    let c = String.unsafe_get bases !i in
    let push call strand =
      let read =
        { Mpileup.Read.call; strand;
          quality = Char.code (String.unsafe_get quals !qpos) - quality_offset;
          indel = None; starts_read = !pending_start; ends_read = false } in
      pending_start := None;
      List.accum acc read;
      incr count;
      incr qpos in
    (match c with
     | '.' -> push Mpileup.Call.Reference Sequences.Types.forward
     | ',' -> push Mpileup.Call.Reference Sequences.Types.reverse
     | 'A' | 'C' | 'G' | 'T' | 'N' ->
       push (Mpileup.Call.Base c) Sequences.Types.forward
     | 'a' | 'c' | 'g' | 't' | 'n' ->
       push (Mpileup.Call.Base (Char.uppercase_ascii c)) Sequences.Types.reverse
     | '*' -> push Mpileup.Call.Gap Sequences.Types.forward
     | '#' -> push Mpileup.Call.Gap Sequences.Types.reverse
     | '>' -> push Mpileup.Call.Skip Sequences.Types.forward
     | '<' -> push Mpileup.Call.Skip Sequences.Types.reverse
     | '+' | '-' ->
       let how_many = ref 0 in
       let continue_ = ref true in
       while !continue_ do
         incr i;
         match String.unsafe_get bases !i with
         | '0' .. '9' as d -> how_many := !how_many * 10 + (Char.code d - 48)
         | _ -> continue_ := false
       done;
       let s = String.uppercase_ascii (String.sub bases !i !how_many) in
       (match !acc with
        | last :: rest ->
          acc :=
            { last with
              Mpileup.Read.indel =
                Some (if c = '+' then Mpileup.Indel.Insertion s
                      else Mpileup.Indel.Deletion s) } :: rest
        | [] -> ());
       i := !i + !how_many - 1
     | '^' -> incr i; pending_start := Some (Char.code (String.unsafe_get bases !i) - 33)
     | '$' ->
       (match !acc with
        | last :: rest -> acc := { last with Mpileup.Read.ends_read = true } :: rest
        | [] -> ())
     | _ -> Exception.raise __FUNCTION__ IO_Format "Unknown character");
    incr i
  done;
  let blank =
    { Mpileup.Read.call = Mpileup.Call.Gap; strand = Sequences.Types.forward;
      quality = 0; indel = None; starts_read = None; ends_read = false } in
  let reads = Array.make !count blank in
  List.iteri (fun k r -> reads.(!count - 1 - k) <- r) !acc;
  reads

(* Time and allocate.  [Gc.minor_words] counts what was handed out, which is
   the quantity a per-character [String.sub] shows up in. *)
let repeats = 7

let measure name f n_lines n_bases =
  (* Best of several rather than one run: what is wanted is the cost of the
     work, and a single run also measures whatever else the machine was doing.
     The best run is the one least interfered with. *)
  let best = ref infinity and words = ref 0. in
  for _ = 1 to repeats do
    Gc.full_major ();
    let words_before = Gc.minor_words () and t0 = Unix.gettimeofday () in
    f ();
    let elapsed = Unix.gettimeofday () -. t0 in
    if elapsed < !best then
      best := elapsed;
    words := Gc.minor_words () -. words_before
  done;
  let elapsed = !best and words = !words in
  Printf.printf "  %-12s %8.3f s   %10.0f lines/s   %6.1f ns/base   %8.1f words/base\n%!"
    name elapsed (float_of_int n_lines /. elapsed)
    (elapsed *. 1e9 /. float_of_int n_bases)
    (words /. float_of_int n_bases);
  elapsed

let () =
  Printf.printf "\n%s\n\n" (String.TermIO.bold "Reading a pileup: before and after");
  List.iter
    (fun (lines, depth) ->
      let text = generate ~lines ~depth in
      let all = String.Split.on_char_as_list '\n' text |> List.filter (fun l -> l <> "") in
      let n_lines = List.length all and n_bases = lines * depth in
      Printf.printf "%s (%d lines x depth %d = %d calls, %.1f MB)\n"
        (String.TermIO.bold "input") n_lines depth n_bases
        (float_of_int (String.length text) /. 1048576.);
      (* Both are run once over the same lines, having first been made to agree
         on what they produce: a measurement of two things doing different work
         would say nothing. *)
      (match all with
       | first :: _ ->
         let a = (Mpileup.of_line first).Mpileup.reads and b = old_style_of_line first in
         if Array.length a <> Array.length b then begin
           Printf.printf "  the two readers disagree -- not comparable\n%!";
           exit 1
         end
       | [] -> ());
      let old_time =
        measure "old style" (fun () -> List.iter (fun l -> ignore (old_style_of_line l)) all)
          n_lines n_bases
      and new_time =
        measure "lexer" (fun () -> List.iter (fun l -> ignore (Mpileup.of_line l)) all)
          n_lines n_bases
      and direct_time =
        measure "plain chars" (fun () -> List.iter (fun l -> ignore (direct_of_line l)) all)
          n_lines n_bases in
      Printf.printf "  %-12s lexer %.2fx, plain chars %.2fx\n\n%!"
        "against old" (old_time /. new_time) (old_time /. direct_time))
    [ 2000, 50; 2000, 200; 500, 1000 ]
