(*
    Tests_Files.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Tests_Files.ml exercises the marshalling of a quoted path.  A
    QuotedPath.t carries a name twice, once as it is and once as a
    shell would need it written, and the pair is marshalled into a
    single tab-separated string.  Both halves are escaped and the
    result escaped again, which looks redundant until the name itself
    contains a tab: without the second pass the separator would be
    indistinguishable from the content and the pair would come back
    split in the wrong place.  Those are the cases checked here.

    The parts of Files that read sequence data are not covered: they
    take channels and file names, and what they do with them is a
    separate concern from what this file is about.

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

module Q = Files.QuotedPath

(* Helpers. *)

let qp unquoted quoted = { Q.unquoted; quoted }

let show (p: Q.t) = Printf.sprintf "%S|%S" p.Q.unquoted p.Q.quoted

(* A path survives marshalling iff both halves come back exactly. *)
let round_trips p = show (Q.of_string (Q.to_string p)) = show p

let test_quoted_path () =
  Testing.section "Quoted paths" (fun () ->
    Testing.check_string "the empty path has two empty halves"
      ~expected:"\"\"|\"\"" (show Q.none);
    Testing.check "the empty path survives marshalling" (fun () -> round_trips Q.none);
    Testing.check "an ordinary path survives"
      (fun () -> round_trips (qp "/tmp/file.txt" "/tmp/file.txt"));
    (* The quoted half is what a shell would need, so it routinely differs from
       the unquoted one; both have to come back. *)
    Testing.check "a path whose halves differ survives"
      (fun () -> round_trips (qp "/tmp/a b" "/tmp/a\\ b"));
    Testing.check_string "and the halves are not transposed"
      ~expected:"\"/tmp/a b\"|\"/tmp/a\\\\ b\""
      (show (Q.of_string (Q.to_string (qp "/tmp/a b" "/tmp/a\\ b"))));
    (* The case the double escaping exists for. *)
    Testing.check "a path containing a tab survives"
      (fun () -> round_trips (qp "/tmp/a\tb" "/tmp/a\\\tb"));
    Testing.check_string "and the tab is still a tab afterwards"
      ~expected:"/tmp/a\tb"
      ((Q.of_string (Q.to_string (qp "/tmp/a\tb" "x"))).Q.unquoted);
    (* Backslashes and newlines are the other two that escaping has to survive
       without either doubling up or collapsing. *)
    Testing.check "a path containing a backslash survives"
      (fun () -> round_trips (qp "/tmp/a\\b" "/tmp/a\\\\b"));
    Testing.check "a path containing a newline survives"
      (fun () -> round_trips (qp "/tmp/a\nb" "/tmp/a\\nb"));
    Testing.check "a path containing a double quote survives"
      (fun () -> round_trips (qp "/tmp/a\"b" "/tmp/a\\\"b"));
    Testing.check "a path containing a single quote survives"
      (fun () -> round_trips (qp "/tmp/a'b" "'/tmp/a'\\''b'"));
    (* Several at once, since escaping bugs tend to show up on interaction. *)
    Testing.check "a path containing a tab and a backslash survives"
      (fun () -> round_trips (qp "a\t\\b" "c\\\td"));
    Testing.check "a path that is nothing but separators survives"
      (fun () -> round_trips (qp "\t\t" "\\t\\t"));
    (* A string that never came from to_string is not a marshalled pair. *)
    Testing.check_raises "a string with no separator is refused"
      (fun () -> ignore (Q.of_string "no-separator-here"));
    Testing.check_raises "a string with too many separators is refused"
      (fun () -> ignore (Q.of_string "a\\tb\\tc")))

(* The sequence readers.  These take a path rather than a string and hand each
   record back with the read's index, its segment index and the record itself,
   so the fixtures go through a file and the check collects what it is given. *)

let with_file text f =
  let path = Filename.temp_file "BiOCamLib_Tests_" ".seq" in
  let oc = open_out path in
  output_string oc text;
  close_out oc;
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () -> f path)

let collect ?linter (reader: string Files.Base.Iterator.t) text =
  with_file text (fun path ->
    let acc = ref [] in
    reader ?linter ~verbose:false
      (fun (i, seg, r) ->
        List.accum acc
          (Printf.sprintf "%d/%d %s|%s|%s"
             i seg r.Files.Base.Read.tag r.Files.Base.Read.seq r.Files.Base.Read.qua))
      path;
    List.rev !acc |> String.concat " ")

let test_sequence_readers () =
  Testing.section "Sequence readers" (fun () ->
    (* Records arrive numbered from zero, each with a segment index that stays
       at zero for single-end input.  A FASTA tag is everything after the [>],
       description and all, and a sequence broken over several lines arrives
       joined -- the line breaks are the file's business, not the record's. *)
    Testing.check_string "FASTA gives tag and joined sequence, and no qualities"
      ~expected:"0/0 a desc|ACGT| 1/0 b|TTTT|"
      (collect Files.FASTA.iter ">a desc\nACGT\n>b\nTT\nTT\n");
    Testing.check_string "FASTQ gives the qualities as well"
      ~expected:"0/0 a|ACGT|IIII 1/0 b|TTTT|JJJJ"
      (collect Files.FASTQ.iter "@a\nACGT\n+\nIIII\n@b\nTTTT\n+\nJJJJ\n");
    (* The tabular form is the same record in three columns, and the point of
       having it is that it reads back as the same record. *)
    Testing.check_string "and tabular gives what FASTQ gave"
      ~expected:(collect Files.FASTQ.iter "@a\nACGT\n+\nIIII\n@b\nTTTT\n+\nJJJJ\n")
      (collect Files.Tabular.iter "a\tACGT\tIIII\nb\tTTTT\tJJJJ\n");
    (* The linter is handed each sequence on the way through, and only the
       sequence: a reader that linted the tag would quietly rename the read. *)
    Testing.check_string "the linter is applied to the sequence"
      ~expected:"0/0 a|acgt|"
      (collect ~linter:String.lowercase_ascii Files.FASTA.iter ">a\nACGT\n");
    Testing.check_string "and not to the tag"
      ~expected:"0/0 A_Tag|acgt|"
      (collect ~linter:String.lowercase_ascii Files.FASTA.iter ">A_Tag\nACGT\n");
    (* Nothing in, nothing out -- rather than one empty record. *)
    Testing.check_string "an empty file yields no records" ~expected:""
      (collect Files.FASTA.iter "");
    Testing.check_string "and so does an empty tabular one" ~expected:""
      (collect Files.Tabular.iter "");
    (* A missing file is the caller's ordinary mistake, and says so. *)
    Testing.check_raises ~re:"Input file not found" "a missing file is refused as such"
      (fun () ->
        Files.FASTA.iter ~verbose:false (fun _ -> ())
          "/nonexistent/BiOCamLib_Tests_missing.fasta"))

(* Transparent decompression.  The readers take a path and decide from the magic
   number whether to spawn a decompressor, so what is checked here is that the
   same records come back however the file is packed, and -- the part that had
   no coverage at all -- that a helper which fails is HEARD.  A corrupt archive
   exits non-zero, and swallowing that is how a short read becomes a short
   dataset with every downstream ratio computed over whatever arrived.

   The compressors are looked up rather than assumed: where they are absent the
   checks say so and are skipped, because a group that quietly ran nothing is
   indistinguishable from one that passed. *)

let have cmd =
  Sys.command (Printf.sprintf "command -v %s >/dev/null 2>&1" cmd) = 0

let with_packed ~how text f =
  let path = Filename.temp_file "BiOCamLib_Tests_" ".seq" in
  let oc = open_out path in
  output_string oc text;
  close_out oc;
  Fun.protect ~finally:(fun () -> List.iter (fun p -> try Sys.remove p with _ -> ()) [ path ])
    (fun () ->
      (* The compressor replaces the file, so the packed one carries the suffix
         it chooses; the reader never looks at the name, only at the bytes *)
      let packed = path ^ (if how = "gzip" then ".gz" else ".bz2") in
      Fun.protect ~finally:(fun () -> try Sys.remove packed with _ -> ())
        (fun () ->
          if Sys.command (Printf.sprintf "%s -c %s > %s" how (Filename.quote path)
                            (Filename.quote packed)) <> 0 then
            failwith (how ^ " failed");
          f packed))

let read_tags path =
  let acc = ref [] in
  Files.FASTA.iter ~verbose:false
    (fun (_, _, r) -> List.accum acc r.Files.Base.Read.tag) path;
  List.rev !acc |> String.concat " "

let test_compression () =
  Testing.section "Transparent decompression" (fun () ->
    let fasta = ">a\nACGT\n>b\nTTTT\n" in
    List.iter
      (fun how ->
        if not (have how) then
          Testing.check_bool
            (Printf.sprintf "SKIPPED: %s is not installed, so its reading is unchecked" how)
            ~expected:true true
        else begin
          Testing.check_string (Printf.sprintf "a %s archive reads as the plain file does" how)
            ~expected:"a b" (with_packed ~how fasta read_tags);
          (* Truncating an archive leaves the decompressor exiting non-zero part
             way through.  What must NOT happen is the reader taking the short
             output for the whole file: the records it did get are perfectly
             well-formed, so nothing about them says anything is missing. *)
          Testing.check_raises
            (Printf.sprintf "and a truncated %s archive is refused, not silently short" how)
            (fun () ->
              with_packed ~how fasta (fun packed ->
                let n = (Unix.stat packed).Unix.st_size in
                let ic = open_in_bin packed in
                let head = really_input_string ic (max 1 (n / 2)) in
                close_in ic;
                let oc = open_out_bin packed in
                output_string oc head;
                close_out oc;
                read_tags packed))
        end)
      [ "gzip"; "bzip2" ];
    (* The case where the two failures coincide, which is the usual one: a
       corrupt archive decodes to malformed content, and the reader used to
       report the content and never reach the helper's exit status at all --
       naming the symptom and swallowing the cause. *)
    (* What is NOT asserted here, deliberately.  A corrupt archive that decodes
       to malformed content reports the content, not the decompressor, and that
       cannot be repaired by closing the helper on the way out: abandoning the
       read closes the pipe, the helper dies of SIGPIPE, and reap() treats that
       as the legitimate early stop it usually is.  Its true exit status is
       destroyed by the very act of asking for it.  The protection that DOES
       hold is the one above -- an archive read to its end is checked -- and it
       is the one that matters, because a truncated archive whose content stays
       well-formed is the case that would otherwise pass silently. *)
    ())


(* GEM MAP reading. *)

let test_gem_map () =
  Testing.section "GEM MAP reading" (fun () ->
    let module G = Files.Gem in
    (* What gem3-mapper -F MAP wrote for a toy reference: a unique read, a read on a repeat placed
       on both copies, a mismatch, a deletion ('>1+'), an insertion ('>1-') -- and an unmapped one.
       The reads are abbreviated; the reader does not check them against the alignments. *)
    let text =
      "r_uniq\tGATTACA\t1:0:0:0:0:0+0\tctgA:+:1:60:::60\n" ^
      "r_motif\tTTGGCC\t2:0:0:0:0+0\tctgA:+:41:30:::0,ctgA:+:111:30:::0\n" ^
      "r_mism\tCAGTCA\t0:1:0:0:0+0\tctgA:+:71:20T19:::60\n" ^
      "r_del\tTACGTA\t0:1:0:0:0+0\tctgA:+:141:20>1+19:::60\n" ^
      "r_ins\tTACGTA\t0:1:0:0:0+0\tctgA:-:141:20>1-20:::60\n" ^
      "r_none\tACGT\t0:0+0\t-\n" in
    let read ?qualities ?strata text =
      let seen = ref [] in
      with_file text (fun path ->
        let ic = open_in path in
        Fun.protect ~finally:(fun () -> close_in ic)
          (fun () ->
            G.iter ?qualities ?strata (fun read ~placements m -> List.accum seen (read.G.tag, placements, m)) ic));
      List.rev !seen in
    let seen = read text in
    let nth i = List.nth seen i in
    let covered i = let _, _, m = nth i in G.Gigar.covered m.G.position m.G.gigar in
    let placements i = let _, n, _ = nth i in n in
    Testing.check_int "every placement is seen once, multimaps included and unmapped reads not"
      ~expected:6 (List.length seen);
    Testing.check_string "the tag comes with each placement" ~expected:"r_motif" (let tag, _, _ = nth 2 in tag);
    Testing.check "each placement comes with how many the read has"
      (fun () -> placements 0 = 1 && placements 1 = 2 && placements 2 = 2 && placements 3 = 1);
    Testing.check "a perfect read covers its whole length" (fun () -> covered 0 = [ 1, 60 ]);
    Testing.check "a read on a repeat is placed on both copies"
      (fun () -> covered 1 = [ 41, 30 ] && covered 2 = [ 111, 30 ]);
    Testing.check "a mismatch consumes one reference base" (fun () -> covered 3 = [ 71, 40 ]);
    Testing.check "a deletion, '>1+', consumes the reference base the read lacks"
      (fun () -> covered 4 = [ 141, 40 ]);
    Testing.check "an insertion, '>1-', consumes no reference" (fun () -> covered 5 = [ 141, 40 ]);
    Testing.check "the contig and the strand are read"
      (fun () -> let _, _, m = nth 0 and _, _, m' = nth 5 in m.G.contig = "ctgA" && m.G.forward && not m'.G.forward);
    Testing.check "a splice breaks the covered span into two intervals"
      (fun () -> G.Gigar.covered 100 [ G.Gigar.Match 10; G.Gigar.Splice 5; G.Gigar.Match 3 ] = [ 100, 10; 115, 3 ]);
    Testing.check "a trim consumes no reference"
      (fun () -> G.Gigar.covered 7 [ G.Gigar.Trim 5; G.Gigar.Match 10; G.Gigar.Trim 2 ] = [ 7, 10 ]);
    Testing.check "records with qualities are read when the caller says so"
      (fun () -> read ~qualities:true "r\tACGT\tIIII\t1+0\tc:+:1:4\n" |> List.length = 1);
    Testing.check_raises "a qualities column not announced is reported, not mistaken for counters"
      (fun () -> read "r\tACGT\tIIII\t1+0\tc:+:1:4\n");
    Testing.check_raises "an alignment string with junk in it is reported"
      (fun () -> read "r\tACGT\t1+0\tc:+:1:2?2\n");
    (* Strata: a read placed once without errors and twice with one *)
    let three = "r\tACGT\t1:2+0\tc:+:1:4:::0,c:+:11:4:::0,c:+:21:4:::0\n" in
    Testing.check "without strata every placement is delivered, and the read knows all three"
      (fun () -> match read three with [ _, 3, _; _, 3, _; _, 3, _ ] -> true | _ -> false);
    Testing.check "the first stratum is the errorless placement alone, and the read knows one"
      (fun () -> match read ~strata:1 three with [ _, 1, m ] -> m.G.position = 1 | _ -> false);
    Testing.check "two strata are all three" (fun () -> List.length (read ~strata:2 three) = 3);
    Testing.check "the first non-empty stratum counts, wherever it sits"
      (fun () -> List.length (read ~strata:1 "r\tACGT\t0:0:2+0\tc:+:1:4:::0,c:+:11:4:::0\n") = 2);
    Testing.check "the counts after the '+' are strata like the others"
      (fun () -> List.length (read ~strata:1 "r\tACGT\t0:0+0:0:2\tc:+:1:4:::0,c:+:11:4:::0\n") = 2);
    Testing.check "'AxB' is the count A repeated B times"
      (fun () -> match read ~strata:1 "r\tACGT\t0x2:1:1+0\tc:+:1:4:::0,c:+:11:4:::0\n" with [ _, 1, _ ] -> true | _ -> false);
    (* A pair: its placements count once with both mates, and are delivered with both *)
    let pair = "p\tACGT GGCC\t1:1+0\tc:+:1:4::c:-:11:4:::0,c:+:21:4::c:-:31:4:::0\n" in
    Testing.check "a placement of a pair is both its mates, counted once"
      (fun () -> match read ~strata:1 pair with [ _, 1, m; _, 1, m' ] -> m.G.position = 1 && m'.G.position = 11 | _ -> false);
    Testing.check "the strata of a pair are its pair placements"
      (fun () -> match read ~strata:2 pair with [ _, 2, _; _, 2, _; _, 2, _; _, 2, _ ] -> true | _ -> false);
    Testing.check "each end of a pair comes with its own mate's bases and qualities"
      (fun () ->
        let seen = ref [] in
        with_file "p\tACGT GGCC\tIIII JJJJ\t1+0\tc:+:1:4::c:-:11:4:::0\n" (fun path ->
          let ic = open_in path in
          Fun.protect ~finally:(fun () -> close_in ic)
            (fun () -> G.iter ~qualities:true (fun r ~placements:_ _ -> List.accum seen (r.G.sequence, r.G.qualities)) ic));
        List.rev !seen = [ "ACGT", Some "IIII"; "GGCC", Some "JJJJ" ]);
    Testing.check_raises "counters that cannot be counted are reported"
      (fun () -> read ~strata:1 "r\tACGT\t!\tc:+:1:4:::0\n");
    Testing.check_raises "counters announcing no placement for a placed read are reported"
      (fun () -> read "r\tACGT\t0:0+0\tc:+:1:4:::0\n"))

let run () =
  test_quoted_path ();
  test_sequence_readers ();
  test_compression ();
  test_gem_map ()
