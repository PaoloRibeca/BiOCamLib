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

(* Counting a line rather than keeping it.  This is the shape a variant caller
   wants, and the thing most worth pinning about it is that the reference has no
   special status: a read that wrote '.' and one that spelled the base out are
   the same vote and must land in the same bucket, or one genotype silently
   becomes two and the reference allele is under-counted. *)

let summarize ?strand s =
  M.summarize ?strand s

let show_genotypes (u: M.Summary.t) =
  List.map
    (fun (g: M.Genotype.t) ->
      Printf.sprintf "%s:%d%s" g.M.Genotype.symbol g.M.Genotype.count
        (match g.M.Genotype.qualities with
         | None -> ""
         | Some q -> Printf.sprintf "@%.4g" (M.Qualities.mean q)))
    u.M.Summary.genotypes
  |> String.concat " "

(* The kind of whichever genotype is an indel, there being at most one in each
   of the lines below. *)
let kind_of_indel (u: M.Summary.t) =
  match
    List.find_opt (fun (g: M.Genotype.t) -> g.M.Genotype.kind <> M.Genotype.Base)
      u.M.Summary.genotypes
  with
  | Some { M.Genotype.kind = M.Genotype.Short_indel; _ } -> "Short_indel"
  | Some { M.Genotype.kind = M.Genotype.Long_indel; _ } -> "Long_indel"
  | _ -> "none"

let test_summary () =
  Testing.section "Pileup summary" (fun () ->
    (* Reference matches and explicit bases are one genotype, not two. *)
    Testing.check_string "a dot and the base it stands for are the same vote"
      ~expected:"A:4@40"
      (show_genotypes (summarize (line [ "c"; "1"; "A"; "4"; ".,Aa"; "IIII" ])));
    Testing.check_string "and a different base is a different vote"
      ~expected:"A:2@40 G:2@40"
      (show_genotypes (summarize (line [ "c"; "1"; "A"; "4"; ".,Gg"; "IIII" ])));
    (* Indels are genotypes of their own, classified by length because the model
       above this gives short and long ones different error rates. *)
    Testing.check_string "an indel is a genotype in its own right"
      ~expected:"A:2@40 +AC:1"
      (show_genotypes (summarize (line [ "c"; "1"; "A"; "2"; ".+2AC,"; "II" ])));
    Testing.check_string "a single base deleted is a short indel"
      ~expected:"Short_indel"
      (kind_of_indel (summarize (line [ "c"; "1"; "A"; "1"; ".-1G"; "I" ])));
    Testing.check_string "and several bases a long one"
      ~expected:"Long_indel"
      (kind_of_indel (summarize (line [ "c"; "1"; "A"; "1"; ".-2GG"; "I" ])));
    Testing.check_bool "an indel carries no quality, rather than a zero one"
      ~expected:true
      (List.for_all
         (fun (g: M.Genotype.t) ->
           match g.M.Genotype.kind, g.M.Genotype.qualities with
           | M.Genotype.Base, Some _ -> true
           | (M.Genotype.Short_indel | M.Genotype.Long_indel), None -> true
           | _ -> false)
         (summarize (line [ "c"; "1"; "A"; "2"; ".+2AC,"; "II" ])).M.Summary.genotypes);
    (* A read inside a deletion or skipping the reference is counted by the
       aligner but votes for nothing. *)
    Testing.check_string "gaps and skips are counted apart from the votes"
      ~expected:"depth 4, voting 1, gaps 2, skips 1"
      (let u = summarize (line [ "c"; "1"; "A"; "4"; ".*#>"; "IIII" ]) in
       Printf.sprintf "depth %d, voting %d, gaps %d, skips %d"
         u.M.Summary.depth u.M.Summary.voting u.M.Summary.gaps u.M.Summary.skips);
    Testing.check_string "and contribute no genotype" ~expected:"A:1@40"
      (show_genotypes (summarize (line [ "c"; "1"; "A"; "4"; ".*#>"; "IIII" ])));
    (* A directional protocol is evidence about one strand only. *)
    Testing.check_string "a strand filter keeps only the reads on it"
      ~expected:"A:1@40 G:1@40"
      (show_genotypes
         (summarize ~strand:Sequences.Types.forward
            (line [ "c"; "1"; "A"; "4"; ".,Gg"; "IIII" ])));
    Testing.check_string "and the other strand sees the others"
      ~expected:"A:1@40 G:1@40"
      (show_genotypes
         (summarize ~strand:Sequences.Types.reverse
            (line [ "c"; "1"; "A"; "4"; ".,Gg"; "IIII" ])));
    Testing.check_string "a line at depth zero has nothing to say" ~expected:""
      (show_genotypes (summarize (line [ "c"; "1"; "A"; "0"; "*"; "*" ]))))

(* The quality histogram, which is what the model's likelihood is computed
   over: a null distribution built by merging every genotype but one, and the
   variant's own with its lowest quarter dropped. *)

let test_qualities () =
  Testing.section "Quality distributions" (fun () ->
    let of_list l =
      let q = M.Qualities.make () in
      List.iter (M.Qualities.add q) l;
      q in
    Testing.check_int "an empty distribution counts nothing" ~expected:0
      (M.Qualities.cardinal (M.Qualities.make ()));
    Testing.check_float "the mean of an empty one is zero" ~expected:0.
      (M.Qualities.mean (M.Qualities.make ()));
    Testing.check_int "what goes in is counted" ~expected:4
      (M.Qualities.cardinal (of_list [ 10; 20; 20; 30 ]));
    Testing.check_float "and averaged" ~expected:20.
      (M.Qualities.mean (of_list [ 10; 20; 20; 30 ]));
    (* The sample variance of 10, 20, 20, 30 is 200/3. *)
    Testing.check_float "the variance is the sample one" ~expected:(200. /. 3.)
      (M.Qualities.variance (of_list [ 10; 20; 20; 30 ]));
    Testing.check_float "one observation has no variance" ~expected:0.
      (M.Qualities.variance (of_list [ 40 ]));
    (* Merging is what builds a variant's null: everything that is not it. *)
    Testing.check_float "merging adds one distribution into another" ~expected:20.
      (let a = of_list [ 10; 30 ] and b = of_list [ 15; 25 ] in
       M.Qualities.merge_into ~into:a b;
       M.Qualities.mean a);
    Testing.check_int "and the counts with it" ~expected:4
      (let a = of_list [ 10; 30 ] and b = of_list [ 15; 25 ] in
       M.Qualities.merge_into ~into:a b;
       M.Qualities.cardinal a);
    (* Dropping the lowest quarter of four observations drops exactly one. *)
    Testing.check_float "the lowest quarter is dropped before averaging"
      ~expected:30. (M.Qualities.mean_above_fraction (of_list [ 10; 20; 30; 40 ]) 0.25);
    Testing.check_float "dropping nothing is the plain mean" ~expected:25.
      (M.Qualities.mean_above_fraction (of_list [ 10; 20; 30; 40 ]) 0.);
    Testing.check_float "and dropping everything leaves nothing" ~expected:0.
      (M.Qualities.mean_above_fraction (of_list [ 10; 20; 30; 40 ]) 1.);
    (* Iteration is what a caller rebuilding a sparse distribution needs, and
       it must offer the qualities that are there and only those: a walk over
       the whole scale would hand back a hundred-odd zeroes to be filtered. *)
    Testing.check_string "iteration gives the qualities present, lowest first"
      ~expected:"10x1 20x2 30x1"
      (let acc = ref [] in
       M.Qualities.iter (fun q c -> acc := Printf.sprintf "%dx%d" q c :: !acc)
         (of_list [ 20; 10; 30; 20 ]);
       List.rev !acc |> String.concat " ");
    Testing.check_int "and skips the empty buckets" ~expected:0
      (let n = ref 0 in
       M.Qualities.iter (fun _ _ -> incr n) (M.Qualities.make ());
       !n);
    Testing.check_int "what it reports sums back to the cardinal" ~expected:4
      (let n = ref 0 in
       M.Qualities.iter (fun _ c -> n := !n + c) (of_list [ 20; 10; 30; 20 ]);
       !n);
    Testing.check_raises "a quality outside the scale is refused"
      (fun () -> M.Qualities.add (M.Qualities.make ()) 200))

(* Agreement with SiNPle, which is the reader this one is meant to replace.
   The lines below were put through SiNPle itself and its output recorded; what
   is checked here is that the counting agrees with it, position by position.
   That is the whole safety of the replacement: the model above is not being
   touched, so if the counts and the mean qualities going into it are the same,
   what comes out of it is too.

   SiNPle printed, for these seven lines in order:

     polio 100 A 4 40   0.999  G   2 40 0.994
     polio 101 C 5 40   1
     polio 102 G 4 40   1
     polio 103 T 4 40   1      +AC 1 0  0.653
     polio 104 A 4 34.8 1      -G  1 0  0.16
     polio 105 C 3 40   1
     polio 106

   -- sequence, position, then four columns per genotype: symbol, count, mean
   sequencing quality, posterior probability.  The posterior is the model's and
   not this reader's business; the symbol, the count and the mean are. *)

let test_agreement () =
  Testing.section "Agreement with SiNPle" (fun () ->
    (* A dot, a comma and a spelled-out base are all votes for a base, and the
       two strands are one genotype: SiNPle counts four As here, not two dots
       and two commas. *)
    Testing.check_string "reference matches on both strands are one genotype"
      ~expected:"A:4@40 G:2@40"
      (show_genotypes (summarize (line [ "polio"; "100"; "A"; "6"; ".,.,Gg"; "IIIIII" ])));
    (* A read starting here votes like any other. *)
    Testing.check_string "a read that starts here still votes"
      ~expected:"C:5@40"
      (show_genotypes (summarize (line [ "polio"; "101"; "C"; "5"; "....^K."; "IIIII" ])));
    Testing.check_string "and one that ends here"
      ~expected:"G:4@40"
      (show_genotypes (summarize (line [ "polio"; "102"; "G"; "4"; ".,.$,"; "IIII" ])));
    (* The read carrying an insertion votes twice: once for the base it agreed
       on, once for the insertion.  SiNPle counts four Ts AND one +AC. *)
    Testing.check_string "a read with an insertion votes for its base and the indel"
      ~expected:"T:4@40 +AC:1"
      (show_genotypes (summarize (line [ "polio"; "103"; "T"; "4"; "..+2AC.."; "IIII" ])));
    Testing.check_string "and likewise with a deletion"
      ~expected:"A:4@34.75 -G:1"
      (show_genotypes (summarize (line [ "polio"; "104"; "A"; "4"; ".,-1G.,"; "IIH5" ])));
    (* The mean of I, I, H and 5 -- 40, 40, 39 and 20 -- is 34.75, which SiNPle
       printed to three figures as 34.8. *)
    Testing.check_float "the mean quality is over every read that voted"
      ~expected:34.75
      (match (summarize (line [ "polio"; "104"; "A"; "4"; ".,-1G.,"; "IIH5" ]))
               .M.Summary.genotypes with
       | { M.Genotype.qualities = Some q; _ } :: _ -> M.Qualities.mean q
       | _ -> nan);
    (* A read inside a deletion from an earlier line votes for nothing: SiNPle
       counts three Cs where the column holds four calls. *)
    Testing.check_string "a gap is not a vote" ~expected:"C:3@40"
      (show_genotypes (summarize (line [ "polio"; "105"; "C"; "4"; ".*.,"; "IIII" ])));
    Testing.check_string "though it is still counted"
      ~expected:"depth 4, voting 3, gaps 1"
      (let u = summarize (line [ "polio"; "105"; "C"; "4"; ".*.,"; "IIII" ]) in
       Printf.sprintf "depth %d, voting %d, gaps %d"
         u.M.Summary.depth u.M.Summary.voting u.M.Summary.gaps);
    (* A position with no coverage has nothing to say about any genotype, which
       is what SiNPle's bare 'polio 106' says too. *)
    Testing.check_string "and a position with no coverage says nothing" ~expected:""
      (show_genotypes (summarize (line [ "polio"; "106"; "T"; "0"; "*"; "*" ]))))

let run () =
  test_columns ();
  test_calls ();
  test_round_trip ();
  test_refusals ();
  test_iteration ();
  test_summary ();
  test_qualities ();
  test_agreement ()
