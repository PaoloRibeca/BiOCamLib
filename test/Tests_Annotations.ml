(*
    Tests_Annotations.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Tests_Annotations.ml exercises the Annotations subsystem and the
    parts of Sequences that AnnoTools leans on.  Checks carrying a
    [~known_bug] marker pin a defect diagnosed in the tabular-mode
    design note (DocsYard/BiOCamLib/docs/BiOCamLib-annotations-tabular.tex);
    they are expected to fail today, and the harness turns one of them
    passing into an error so that a fix cannot land unnoticed.

    Inputs are built inline rather than read from test/ so that each
    check states the exact record it is about.

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

module A = Annotations
module T = Sequences.Types

(* Helpers. *)

let lines = String.concat "\n"

(* Render a parsed LOCATION as "lo..hi,lo..hi" in 1-based inclusive form, with a
   zero-length interval shown as "lo^" so the degenerate case stays visible
   instead of collapsing into a reversed range. *)
let intervals_to_string ivs =
  List.map (fun ((_ : string option), (i : T.simple_interval_t)) ->
    if i.length = 0 then Printf.sprintf "%d^" i.low
    else Printf.sprintf "%d..%d" (i.low + 1) (i.low + i.length)) ivs
  |> String.concat ","

let strand_to_string = function
  | Some (T.Forward _) -> "+"
  | Some (T.Reverse _) -> "-"
  | None -> "."

let location s = A.GenBankLocation.of_string s |> A.GenBankLocation.intervals

(* Wrap a FEATURES block into a minimal but complete GenBank record. *)
let genbank ?(seq = "atgcccgggtaagcgactagcgcatcgtca") feature_lines =
  lines ([
    "LOCUS       demo01     30 bp    DNA              circular UNK";
    "DEFINITION  Synthetic test entry.";
    "ACCESSION   demo01";
    "FEATURES             Location/Qualifiers";
    "     source          1..30";
    "                     /organism=\"synthetic\"" ]
    @ feature_lines
    @ [ "ORIGIN      ";
        Printf.sprintf "        1 %s" seq;
        "//";
        "" ])

let gff3 rows = lines ("##gff-version 3" :: rows @ [ "" ])

(* Every feature of the annotation as (path, feature) pairs, in iteration
   order. *)
let features ann =
  let acc = ref [] in
  A.Annotation.iter_paths (fun ~path feature -> List.accum acc (path, feature)) ann;
  List.rev !acc

let feature_at ann category =
  List.find_opt (fun (path, _) -> List.mem category path) (features ann)

let count_substring needle haystack =
  let n = String.length needle and l = String.length haystack in
  let rec walk i acc =
    if i + n > l then acc
    else if String.sub haystack i n = needle then walk (i + n) (acc + 1)
    else walk (i + 1) acc in
  if n = 0 then 0 else walk 0 0

(* GenBank LOCATION parsing. *)

let test_locations () =
  Testing.section "GenBank LOCATION parsing" (fun () ->
    Testing.check_string "a plain range is 0-based half-open internally"
      ~expected:"1..15" (location "1..15" |> fst |> intervals_to_string);
    Testing.check_string "complement() sets the feature strand"
      ~expected:"-" (location "complement(16..30)" |> snd |> strand_to_string);
    Testing.check_string "join() yields one interval per part"
      ~expected:"1..9,20..28" (location "join(1..9,20..28)" |> fst |> intervals_to_string);
    Testing.check_string "complement(join(...)) keeps the parts in genomic order"
      ~expected:"1..9,20..28"
      (location "complement(join(1..9,20..28))" |> fst |> intervals_to_string);
    (* The reader is right here: a site between 1-based bases 100 and 101 is the
       0-based half-open interval [100, 100).  The defect is in the writers,
       which have no zero-length case and emit the reversed range 101..100. *)
    Testing.check_string "a between-bases site is stored as a zero-length interval"
      ~expected:"100^" (location "100^101" |> fst |> intervals_to_string);
    (* An origin-spanning join on a circular chromosome is stored in feature
       order, NOT sorted.  This check exists to stop anyone "fixing" the
       per-part strand bug below by normalising intervals to ascending order,
       which would silently produce the wrong protein here. *)
    Testing.check_string "an origin-spanning join keeps its parts in feature order"
      ~expected:"30..35,1..5"
      (location "complement(join(30..35,1..5))" |> fst |> intervals_to_string);
    (* [Remote] is [accession * version option * t], so the version is parsed
       off into its own slot; [intervals] projects only the accession. *)
    Testing.check_string "a remote reference records the foreign accession"
      ~expected:"J00194"
      (match location "J00194.1:100..202" |> fst with
       | (Some acc, _) :: _ -> acc
       | _ -> "(none)");
    Testing.check_string "a join whose parts are each complemented is one reverse feature"
      ~expected:"-"
      (location "join(complement(20..28),complement(1..9))" |> snd |> strand_to_string);
    (* Trans-splicing: one part forward, one part reverse.  A feature_t carries a
       single strand, so this is not representable and is refused.  It used to
       keep whichever strand came last, which silently reverse-complemented the
       parts that disagreed with it -- a wrong answer rather than no answer.
       Representing it properly would need a strand per interval. *)
    Testing.check_raises ~re:".*[Mm]ixed-strand.*"
      "a mixed-strand join is refused rather than silently flattened"
      (fun () -> location "join(1..9,complement(20..28))"))

(* GenBank record to AST. *)

let test_genbank_records () =
  Testing.section "GenBank record -> AST" (fun () ->
    let ann =
      genbank [
        "     gene            1..15";
        "                     /gene=\"abcA\"";
        "                     /locus_tag=\"DEMO_0001\"";
        "     CDS             1..15";
        "                     /gene=\"abcA\"";
        "                     /codon_start=\"2\"";
        "                     /product=\"hypothetical\"" ]
      |> A.GenBank.of_string in
    Testing.check_int "every feature of the record is loaded"
      ~expected:3 (List.length (features ann));
    Testing.check_string "the feature id is derived from /locus_tag"
      ~expected:"DEMO_0001"
      (match feature_at ann "gene" with
       | Some (_, f) -> Option.value ~default:"(none)" f.A.Annotation.id
       | None -> "(no gene)");
    Testing.check_string "every GenBank feature is pinned under source"
      ~expected:"annotation->source->CDS"
      (match feature_at ann "CDS" with
       | Some (path, _) -> A.Annotation.path_to_string path
       | None -> "(no CDS)");
    (* /codon_start is the GenBank spelling of GFF3's phase, 1-based against the
       0-based phase.  It is currently never read, so a CDS that does not start
       in frame 0 fails validate_translation for the wrong reason. *)
    (* /codon_start is 1-based against the 0-based phase, so 2 means phase 1. *)
    Testing.check_equal "/codon_start=2 becomes phase 1"
      ~to_string:(function Some n -> string_of_int n | None -> "none")
      ~expected:(Some 1)
      (match feature_at ann "CDS" with
       | Some (_, f) -> f.A.Annotation.phase
       | None -> None);
    Testing.check_equal "a CDS with no /codon_start has no phase"
      ~to_string:(function Some n -> string_of_int n | None -> "none")
      ~expected:None
      (let plain =
         genbank [ "     CDS             1..15"; "                     /gene=\"abcA\"" ]
         |> A.GenBank.of_string in
       match feature_at plain "CDS" with
       | Some (_, f) -> f.A.Annotation.phase
       | None -> Some 99);
    Testing.check_raises ~re:".*codon_start.*"
      "an out-of-range /codon_start is refused"
      (fun () ->
        genbank [
          "     CDS             1..15";
          "                     /codon_start=\"7\"" ]
        |> A.GenBank.of_string))

(* GenBank round trip.  Unlike GFF3, GenBank keeps a joined feature as one
   feature and can spell every location this AST can hold, so it is the format
   whose round trip should be exact. *)

let test_genbank_round_trip () =
  Testing.section "GenBank round trip" (fun () ->
    let round_trip feature_lines =
      let once = genbank feature_lines |> A.GenBank.of_string in
      A.GenBank.to_string once, A.GenBank.to_string (A.GenBank.of_string (A.GenBank.to_string once)) in
    let locations text =
      List.filter_map (fun l ->
        let l = String.trim l in
        match String.Split.on_char_as_list ' ' l with
        | k :: rest when k = "CDS" || k = "misc_feature" ->
          Some (String.concat "" rest)
        | _ -> None) (String.Split.on_char_as_list '\n' text) in
    let cases = [
      "a plain range", [ "     CDS             1..15" ], "1..15";
      "a reverse range", [ "     CDS             complement(16..30)" ], "complement(16..30)";
      "a join", [ "     CDS             join(1..9,20..28)" ], "join(1..9,20..28)";
      "a reverse join",
        [ "     CDS             complement(join(1..9,20..28))" ],
        "complement(join(1..9,20..28))";
      (* The writer used to render this as the reversed range 101..100, which its
         own reader then refused. *)
      "a zero-length site", [ "     misc_feature    100^101" ], "100^101"
    ] in
    List.iter (fun (name, lines_, expected) ->
      let first, second = round_trip lines_ in
      Testing.check_string (Printf.sprintf "%s survives the writer" name)
        ~expected (locations first |> String.concat ",");
      Testing.check_string (Printf.sprintf "%s is stable on a second pass" name)
        ~expected:first second) cases;
    (* /codon_start is now read into phase, and has to come back out again. *)
    let with_phase, _ =
      round_trip [
        "     CDS             1..15";
        "                     /codon_start=\"2\"" ] in
    Testing.check "a /codon_start survives the writer"
      (fun () -> count_substring "/codon_start=\"2\"" with_phase = 1))

(* Feature sequence extraction.  The demo ORIGIN is
   atgcccgggtaagcgactagcgcatcgtca, 30 bases, and the reference the GenBank
   reader builds from it is upper-cased by the default linter. *)

let test_feature_sequence () =
  Testing.section "Feature sequence extraction" (fun () ->
    let cds location extra =
      genbank ([ Printf.sprintf "     CDS             %s" location ] @ extra)
      |> A.GenBank.of_string in
    let dna ann =
      match feature_at ann "CDS" with
      | Some (_, f) -> A.Annotation.feature_dna ann f
      | None -> "(no CDS)"
    and protein ann =
      match feature_at ann "CDS" with
      | Some (_, f) -> A.Annotation.feature_protein ann f
      | None -> "(no CDS)" in
    Testing.check_string "a forward feature yields its own bases"
      ~expected:"ATGCCCGGGTAAGCG" (dna (cds "1..15" []));
    (* rc is checked independently below, so using it here keeps this check
       about the strand handling rather than about the complement table. *)
    Testing.check_string "a reverse feature is reverse-complemented"
      ~expected:(Sequences.Lint.rc "ACTAGCGCATCGTCA") (dna (cds "complement(16..30)" []));
    Testing.check_string "a joined feature is stitched in interval order"
      ~expected:"ATGCCCGGGGCGCATCGT" (dna (cds "join(1..9,20..28)" []));
    Testing.check_string "translation stops at the first in-frame stop"
      ~expected:"MPG" (protein (cds "1..15" []));
    Testing.check_string "a joined CDS translates across the junction"
      ~expected:"MPGAHR" (protein (cds "join(1..9,20..28)" []));
    (* The table is the feature's /transl_table when it carries one.  Table 2
       reads AGA/AGG as stops, which Table 1 does not, so the two disagree on a
       sequence containing one. *)
    Testing.check_string "the translation table defaults to the standard code"
      ~expected:"1"
      (Sequences.Translation.to_string
         (match feature_at (cds "1..15" []) "CDS" with
          | Some (_, f) -> A.Annotation.feature_table (cds "1..15" []) f
          | None -> Sequences.Translation.Table_1));
    Testing.check_string "/transl_table overrides the default"
      ~expected:"11"
      (let ann = cds "1..15" [ "                     /transl_table=\"11\"" ] in
       Sequences.Translation.to_string
         (match feature_at ann "CDS" with
          | Some (_, f) -> A.Annotation.feature_table ann f
          | None -> Sequences.Translation.Table_1));
    (* Extraction needs a reference; asking without one is a programmer error,
       not a data error, so it raises rather than returning "". *)
    Testing.check_raises ~re:".*no reference set.*"
      "extracting without a reference raises"
      (fun () ->
        let ann = A.GFF3.of_string (gff3 [ "chr1\tdemo\tgene\t1\t9\t.\t+\t.\tID=g1" ]) in
        match feature_at ann "gene" with
        | Some (_, f) -> A.Annotation.feature_dna ann f
        | None -> ""))

(* Feature selection. *)

let test_selection () =
  Testing.section "Feature selection" (fun () ->
    let ann =
      genbank [
        "     gene            1..15";
        "                     /gene=\"abcA\"";
        "                     /locus_tag=\"DEMO_0001\"";
        "     CDS             1..15";
        "                     /gene=\"abcA\"";
        "                     /product=\"hypothetical\"";
        "     gene            complement(16..30)";
        "                     /gene=\"xyzB\"";
        "                     /locus_tag=\"DEMO_0002\"" ]
      |> A.GenBank.of_string in
    let count s = A.Selection.count ann s in
    let re f r = A.Selection.Regexps [ f, Str.regexp r ] in
    Testing.check_int "the default selection matches every feature"
      ~expected:4 (count A.Selection.All);
    Testing.check_int "a structural field selects on the leaf category"
      ~expected:1 (count (re "type" "CDS"));
    Testing.check_int "a structural field selects on the strand"
      ~expected:1 (count (re "strand" "-"));
    Testing.check_int "an unknown field name is read as an attribute key"
      ~expected:1 (count (re "gene" "xyz"));
    Testing.check_int "an attribute absent from a feature does not match it"
      ~expected:2 (count (re "locus_tag" "DEMO"));
    Testing.check_int "an empty field name matches the feature label"
      ~expected:1 (count (re "" "DEMO_0002"));
    Testing.check_int "labels select by feature id"
      ~expected:2 (count (A.Selection.Labels (StringSet.of_list [ "DEMO_0001"; "DEMO_0002" ])));
    (* Several criteria are ANDed, so adding one can only narrow the result. *)
    Testing.check_int "several criteria are ANDed"
      ~expected:1
      (count (A.Selection.Regexps [ "type", Str.regexp "gene"; "gene", Str.regexp "abc" ]));
    Testing.check_int "negation is the complement within the register"
      ~expected:3 (count (A.Selection.Not (re "type" "CDS")));
    Testing.check_int "negating everything selects nothing"
      ~expected:0 (count (A.Selection.Not A.Selection.All));
    (* The regexps are Str's, so they are unanchored substring searches -- worth
       pinning, because it is the difference between "gene" matching only the
       gene category and it also matching a category containing that word. *)
    Testing.check_int "a regexp is an unanchored search"
      ~expected:4 (count (re "path" "source"));
    Testing.check_string "a criterion describes itself for diagnostics"
      ~expected:"not (regexps {type})"
      (A.Selection.to_string (A.Selection.Not (re "type" "CDS"))))

(* Attribute handling. *)

let test_attributes () =
  Testing.section "Attribute handling" (fun () ->
    let ann =
      gff3 [ "chr1\tdemo\tgene\t100\t500\t.\t+\t.\tID=g1;Note=alpha%2Cbeta;Desc=x%20y" ]
      |> A.GFF3.of_string in
    let attr key =
      match feature_at ann "gene" with
      | Some (_, f) ->
        (match A.Annotation.attr_get ann f key with
         | Some (v :: _) -> v
         | _ -> "(absent)")
      | None -> "(no gene)" in
    Testing.check_string "percent-encoded values are decoded on read"
      ~expected:"alpha,beta" (attr "Note");
    Testing.check_string "a percent-encoded space is decoded on read"
      ~expected:"x y" (attr "Desc");
    (* Decoding without re-encoding was lossy: the comma re-emerged as a value
       separator on the next read, so the value silently split in two. *)
    Testing.check "a comma inside a value is re-encoded on write"
      (fun () -> count_substring "%2C" (A.GFF3.to_string ann) > 0);
    (* The property that actually matters is not which bytes get encoded but
       that a value survives being written and read again. *)
    let round_tripped key =
      let again = A.GFF3.to_string ann |> A.GFF3.of_string in
      match feature_at again "gene" with
      | Some (_, f) ->
        (match A.Annotation.attr_get again f key with
         | Some vs -> String.concat "|" vs
         | None -> "(absent)")
      | None -> "(no gene)" in
    Testing.check_string "a value containing a comma survives a round trip"
      ~expected:"alpha,beta" (round_tripped "Note");
    Testing.check_string "a value containing a space survives a round trip"
      ~expected:"x y" (round_tripped "Desc");
    (* Column 9 has no way to spell a valueless qualifier other than a bare '=',
       so the grammar has to accept one: a GenBank /pseudo or
       /ribosomal_slippage could otherwise be written but not read back. *)
    Testing.check_does_not_raise "an empty attribute value can be read back"
      (fun () ->
        gff3 [ "chr1\tdemo\tgene\t100\t500\t.\t+\t.\tID=g1;pseudo=" ] |> A.GFF3.of_string);
    Testing.check_string "an empty attribute value reads back as present-but-empty"
      ~expected:""
      (let one =
         gff3 [ "chr1\tdemo\tgene\t100\t500\t.\t+\t.\tID=g1;pseudo=" ] |> A.GFF3.of_string in
       match feature_at one "gene" with
       | Some (_, f) ->
         (match A.Annotation.attr_get one f "pseudo" with
          | Some (v :: _) -> v
          | Some [] -> "(no values)"
          | None -> "(absent)")
       | None -> "(no gene)");
    (* A separator that arrives percent-encoded must not be decoded into a
       structural role: %3D is a literal '=' inside a value, not the pair's
       separator. *)
    Testing.check_string "an encoded separator stays inside the value"
      ~expected:"a=b;c"
      (let tricky =
         gff3 [ "chr1\tdemo\tgene\t100\t500\t.\t+\t.\tID=g1;Note=a%3Db%3Bc" ]
         |> A.GFF3.of_string in
       let again = A.GFF3.to_string tricky |> A.GFF3.of_string in
       match feature_at again "gene" with
       | Some (_, f) ->
         (match A.Annotation.attr_get again f "Note" with
          | Some (v :: _) -> v
          | _ -> "(absent)")
       | None -> "(no gene)");
    (* Reading a value that arrives with an UNENCODED space is a separate
       matter, and this lexer cannot: it treats a space as a token separator,
       so two adjacent values with no comma between them are a parse error.
       Third-party GFF3 carries such values routinely (product=hypothetical
       protein), so this is worth pinning -- but note that it fails loudly
       rather than corrupting, and that our own writer now encodes the space,
       so it costs nothing on a file this library produced. *)
    Testing.check_does_not_raise
      ~known_bug:"the gff_attributes lexer skips spaces, so a value cannot contain one"
      "a value with an unencoded space can be read"
      (fun () ->
        gff3 [ "chr1\tdemo\tgene\t100\t500\t.\t+\t.\tproduct=hypothetical protein" ]
        |> A.GFF3.of_string);
    (* The same lexer rule silently swallows the space in a comma-separated
       list, which is corruption rather than a refusal, and so is the more
       dangerous half of the same defect. *)
    Testing.check_string
      ~known_bug:"the gff_attributes lexer eats the space after a comma"
      "a space after a comma is not swallowed"
      ~expected:"alpha| beta"
      (let one =
         gff3 [ "chr1\tdemo\tgene\t100\t500\t.\t+\t.\tName=alpha, beta" ] |> A.GFF3.of_string in
       match feature_at one "gene" with
       | Some (_, f) ->
         (match A.Annotation.attr_get one f "Name" with
          | Some vs -> String.concat "|" vs
          | None -> "(absent)")
       | None -> "(no gene)"))

(* GFF3 fidelity. *)

let test_gff3_fidelity () =
  Testing.section "GFF3 fidelity" (fun () ->
    let scored =
      gff3 [ "chr1\tdemo\tgene\t100\t500\t42.5\t+\t.\tID=g1" ] |> A.GFF3.of_string in
    Testing.check "the score column survives a round trip"
      (fun () -> count_substring "42.5" (A.GFF3.to_string scored) > 0);
    Testing.check_equal "a score reads back as a float"
      ~to_string:(function Some f -> string_of_float f | None -> "none")
      ~expected:(Some 42.5)
      (match feature_at scored "gene" with
       | Some (_, f) -> f.A.Annotation.score
       | None -> None);
    (* '.' is "no score", which is not the same as a score of zero. *)
    Testing.check_equal "an absent score stays absent"
      ~to_string:(function Some f -> string_of_float f | None -> "none")
      ~expected:None
      (let unscored =
         gff3 [ "chr1\tdemo\tgene\t100\t500\t.\t+\t.\tID=g1" ] |> A.GFF3.of_string in
       match feature_at unscored "gene" with
       | Some (_, f) -> f.A.Annotation.score
       | None -> Some 0.);
    Testing.check_raises ~re:".*[Ss]core.*" "a non-numeric score is refused"
      (fun () ->
        gff3 [ "chr1\tdemo\tgene\t100\t500\tnonsense\t+\t.\tID=g1" ] |> A.GFF3.of_string);
    (* A joined CDS is ONE feature.  The GFF3 writer emits one row per interval
       and the reader never re-merges rows sharing an ID, so it comes back as
       N separate features. *)
    let joined =
      genbank [
        "     CDS             join(1..9,20..28)";
        "                     /gene=\"abcA\"";
        "                     /locus_tag=\"DEMO_0001\"" ]
      |> A.GenBank.of_string in
    Testing.check_int "a joined CDS is a single feature with two intervals"
      ~expected:2
      (match feature_at joined "CDS" with
       | Some (_, f) -> List.length f.A.Annotation.intervals
       | None -> 0);
    let written = A.GFF3.to_string joined in
    Testing.check
      ~known_bug:"row_of_feature emits one row per interval; walk_dfs never re-merges"
      "a joined CDS is written as one GFF3 row"
      (fun () -> count_substring "\tCDS\t" written = 1);
    (* The feature's identity -- here derived from /locus_tag, not carried as an
       ID attribute -- is never written, so nothing downstream can rejoin the
       rows above into the one feature they came from. *)
    Testing.check
      ~known_bug:"row_of_feature emits neither ID= nor Parent=, so identity is lost"
      "a feature id derived from /locus_tag is written as ID="
      (fun () -> count_substring "ID=" written > 0);
    (* Every GenBank feature lives at annotation->source->X, and an Annotation.t
       holds exactly one hierarchy, so GFF3 output of a GenBank register cannot
       be read back at all.  This is the structural reason GFF3 cannot serve as
       the register's text twin. *)
    Testing.check_does_not_raise
      ~known_bug:"GenBank pins features at depth 3; GFF3's default hierarchy rejects them"
      "a GenBank register survives a GFF3 round trip"
      (fun () -> A.GFF3.to_string joined |> A.GFF3.of_string))

(* Attribute ordering. *)

let test_attribute_order () =
  Testing.section "Attribute ordering" (fun () ->
    let ann =
      genbank [
        "     CDS             1..15";
        "                     /product=\"hypothetical\"";
        "                     /gene=\"abcA\"" ]
      |> A.GenBank.of_string in
    let keys = ref [] in
    (match feature_at ann "CDS" with
     | Some (_, f) -> A.Annotation.attr_iter ann (fun k _ -> List.accum keys k) f
     | None -> ());
    (* attrs_of_qualifiers folds a StringMap and AttrMap is keyed by the
       resulting intern ids, so what comes out is global first-intern order,
       not the order the qualifiers appeared in.  Preserving true per-feature
       source order would take a list rather than a map, so this is pinned
       rather than fixed -- and a format that wants a predictable order has to
       sort explicitly, which is what the tabular writer does. *)
    Testing.check_string
      ~known_bug:"AttrMap is keyed by intern id, so per-feature source order is not kept"
      "attributes are emitted in source order"
      ~expected:"product,gene" (List.rev !keys |> String.concat ",");
    (* What IS guaranteed is that the order is deterministic: the same input
       gives the same output, which is what a diffable text format needs. *)
    Testing.check "attribute order is at least deterministic"
      (fun () ->
        let keys_of a =
          let acc = ref [] in
          (match feature_at a "CDS" with
           | Some (_, f) -> A.Annotation.attr_iter a (fun k _ -> List.accum acc k) f
           | None -> ());
          List.rev !acc in
        let build () =
          genbank [
            "     CDS             1..15";
            "                     /product=\"hypothetical\"";
            "                     /gene=\"abcA\"" ]
          |> A.GenBank.of_string in
        keys_of (build ()) = keys_of (build ())))

(* The tabular format. *)

(* Reverse the DATA rows of one [#!] section of a tabular document, leaving its
   banner and header row where they are.  Used to show that row order carries no
   meaning: the parent column is what rebuilds the forest. *)
let reverse_section wanted doc =
  let section = ref "" and header_seen = ref false and out = ref [] and held = ref [] in
  let release () =
    List.iter (fun l -> List.accum out l) !held;
    held := [] in
  List.iter (fun line ->
    let is_banner = String.length line > 2 && String.sub line 0 2 = "#!" in
    if is_banner then begin
      release ();
      section := (match String.Split.on_char_as_list ' ' (String.sub line 2 (String.length line - 2)) with
                  | n :: _ -> n
                  | [] -> "");
      header_seen := false;
      List.accum out line
    end else if !section = wanted && line <> "" then begin
      if not !header_seen then begin
        header_seen := true;
        List.accum out line
      end else
        (* Accumulated in reverse, then released in that order. *)
        held := line :: !held
    end else begin
      release ();
      List.accum out line
    end) (String.Split.on_char_as_list '\n' doc);
  release ();
  List.rev !out |> String.concat "\n"

let test_tabular () =
  Testing.section "Tabular format" (fun () ->
    let describe ann =
      (* A canonical summary of everything a feature carries, so that a round
         trip is compared on content rather than on byte-identical output. *)
      let acc = ref [] in
      A.Annotation.iter_paths (fun ~path f ->
        let attrs = ref [] in
        A.Annotation.attr_iter ann (fun k vs -> List.accum attrs (k ^ "=" ^ String.concat "," vs)) f;
        List.accum acc
          (Printf.sprintf "%s|%s|%s|%s|%s|%s|%s"
             (A.Annotation.path_to_string path) (A.Annotation.seq_name ann f)
             (List.map (fun (i: T.simple_interval_t) ->
                Printf.sprintf "%d+%d" i.low i.length) f.A.Annotation.intervals
              |> String.concat ",")
             (match f.A.Annotation.strand with
              | Some (T.Forward _) -> "+" | Some (T.Reverse _) -> "-" | None -> ".")
             (match f.A.Annotation.phase with Some n -> string_of_int n | None -> ".")
             (match f.A.Annotation.score with Some s -> string_of_float s | None -> ".")
             (List.sort compare !attrs |> String.concat ";"))) ann;
      List.rev !acc |> String.concat "\n" in
    let round_trip ann = A.Tabular.to_string ann |> A.Tabular.of_string in
    let check_round_trip name ann =
      Testing.check_string (Printf.sprintf "%s survives a tabular round trip" name)
        ~expected:(describe ann) (describe (round_trip ann));
      Testing.check_string (Printf.sprintf "%s renders identically on a second pass" name)
        ~expected:(A.Tabular.to_string ann) (A.Tabular.to_string (round_trip ann)) in
    let gb =
      genbank [
        "     gene            1..15";
        "                     /gene=\"abcA\"";
        "                     /locus_tag=\"DEMO_0001\"";
        "     CDS             join(1..9,20..28)";
        "                     /gene=\"abcA\"";
        "                     /codon_start=\"2\"";
        "                     /product=\"hypothetical protein, putative\"";
        "     misc_feature    10^11";
        "                     /note=\"insertion site\"" ]
      |> A.GenBank.of_string in
    check_round_trip "a GenBank register" gb;
    let gf =
      gff3 [
        "chr1\tdemo\tgene\t100\t500\t42.5\t+\t.\tID=gene1;Name=ABC1";
        "chr1\tdemo\tmRNA\t100\t500\t.\t+\t.\tID=mRNA1;Parent=gene1";
        "chr1\tdemo\texon\t100\t200\t.\t+\t.\tID=ex1;Parent=mRNA1";
        "chr1\tdemo\tCDS\t150\t200\t.\t+\t0\tID=cds1;Parent=mRNA1" ]
      |> A.GFF3.of_string in
    check_round_trip "a GFF3 register" gf;
    (* The three things GFF3's column 9 cannot express, which is most of why
       this format exists. *)
    let awkward =
      gff3 [ "chr1\tdemo\tgene\t100\t500\t.\t+\t.\t\
              ID=g1;Note=alpha%2Cbeta;multi=a,b,c;pseudo=;dot=%2E" ]
      |> A.GFF3.of_string in
    check_round_trip "values a packed column cannot hold" awkward;
    Testing.check_string "a multi-valued attribute stays multi-valued"
      ~expected:"a,b,c"
      (let back = round_trip awkward in
       match feature_at back "gene" with
       | Some (_, f) ->
         (match A.Annotation.attr_get back f "multi" with
          | Some vs -> String.concat "," vs
          | None -> "(absent)")
       | None -> "(no gene)");
    Testing.check_string "a value that is a bare dot is not read as absent"
      ~expected:"."
      (let back = round_trip awkward in
       match feature_at back "gene" with
       | Some (_, f) ->
         (match A.Annotation.attr_get back f "dot" with
          | Some (v :: _) -> v
          | _ -> "(absent)")
       | None -> "(no gene)");
    (* The hierarchy travels in the metadata table, so a register read back does
       not need it supplied again -- without that the format would be convenient
       rather than lossless. *)
    Testing.check_string "the hierarchy travels with the data"
      ~expected:(A.Hierarchy.to_string (A.Annotation.hierarchy gb))
      (A.Hierarchy.to_string (A.Annotation.hierarchy (round_trip gb)));
    (* The format reserves a [!] namespace for its own metadata keys and uses
       [#!] section banners in the one-document form, so an annotation's own key
       must not be able to impersonate either.  [add_metadata] accepts any key
       and GFF3 pragmas reach it, so both are constructible. *)
    List.iter (fun hostile_key ->
      Testing.check_string
        (Printf.sprintf "a metadata key of %S cannot impersonate the format's own" hostile_key)
        ~expected:"trap"
        (let hostile = A.Annotation.add_metadata gb ~key:hostile_key ~value:"trap" in
         match A.Annotation.get_metadata (round_trip hostile) hostile_key with
         | v :: _ -> v
         | [] -> "(absent)")) [ "#!features"; "!hierarchy"; "!format-version" ];
    Testing.check_string "annotation metadata travels with the data"
      ~expected:"Synthetic test entry."
      (match A.Annotation.get_metadata (round_trip gb) "DEFINITION" with
       | v :: _ -> v
       | [] -> "(absent)");
    (* Identity is chained through the parent, so two identical exons under two
       different transcripts are distinguishable -- the case that makes a hash
       of the feature alone insufficient, and that GENCODE hits constantly. *)
    let shared =
      gff3 [
        "chr1\tdemo\tgene\t100\t500\t.\t+\t.\tID=g1";
        "chr1\tdemo\tmRNA\t100\t500\t.\t+\t.\tID=t1;Parent=g1";
        "chr1\tdemo\texon\t100\t200\t.\t+\t.\tID=e1;Parent=t1";
        "chr1\tdemo\tmRNA\t100\t500\t.\t+\t.\tID=t2;Parent=g1";
        "chr1\tdemo\texon\t100\t200\t.\t+\t.\tID=e2;Parent=t2" ]
      |> A.GFF3.of_string in
    Testing.check "identical exons under different transcripts get different ids"
      (fun () ->
        let ids =
          String.Split.on_char_as_list '\n' (A.Tabular.to_string shared)
          |> List.filter_map (fun l ->
            match String.Split.on_char_as_list '\t' l with
            | id :: _ :: _ :: path :: _ when path = "gene->mRNA->exon" -> Some id
            | _ -> None) in
        List.length ids = 2 && List.nth ids 0 <> List.nth ids 1);
    check_round_trip "a register with shared exons" shared;
    (* Row order carries no meaning: the parent column is what rebuilds the
       forest, so either file may be sorted and still read back. *)
    (* Reordering the rows reorders the siblings that come back -- the forest is
       rebuilt from the parent links, and nothing records which sibling came
       first -- so the claim being checked is that the same features return, not
       that they return in the same order. *)
    let as_set d = String.Split.on_char_as_list '\n' d |> List.sort compare |> String.concat "\n" in
    Testing.check_string "the features table may be reordered"
      ~expected:(as_set (describe shared))
      (as_set
         (describe (A.Tabular.of_string (reverse_section "features" (A.Tabular.to_string shared)))));
    Testing.check_string "the attributes table may be reordered"
      ~expected:(describe shared)
      (describe
         (A.Tabular.of_string (reverse_section "attributes" (A.Tabular.to_string shared))));
    (* A malformed table is refused rather than half-read. *)
    Testing.check_raises ~re:".*expected header.*"
      "a table with the wrong header is refused"
      (fun () ->
        A.Tabular.of_string
          "#!metadata\nkey\tvalue\n#!features\nwrong\theader\n#!attributes\nid\tkey\tvalue\n");
    (* A row the walk never arrives at, and an attributes row attaching to
       nothing, would both be dropped without a word.  The house rule is that a
       defined format never fails silently. *)
    let doc_with rows attrs =
      String.concat "\n"
        ([ "#!annotation-tabular 1"; "#!metadata"; "key\tvalue"; "!format-version\t1";
           "!hierarchy\t(source (gene, CDS))"; "#!features";
           "id\tparent\tseq\tpath\tfeature_id\tsource\tscore\tstrand\tphase\tintervals" ]
         @ rows @ [ "#!attributes"; "id\tkey\tvalue" ] @ attrs @ [ "" ]) in
    Testing.check_raises ~re:".*unreachable.*"
      "a feature whose parent is not in the table is refused"
      (fun () ->
        A.Tabular.of_string
          (doc_with
             [ "aaaaaaaaaaaaaaaa\t.\tdemo01\tsource\t.\t.\t.\t.\t.\t1..30";
               "bbbbbbbbbbbbbbbb\tnosuchparent\tdemo01\tsource->gene\t.\t.\t.\t.\t.\t1..15" ] []));
    (* A two-cycle has no root, so neither node is ever reached: the
       completeness check is what catches it, not the reached-twice guard. *)
    Testing.check_raises ~re:".*unreachable.*"
      "a cycle in the parent links is refused"
      (fun () ->
        A.Tabular.of_string
          (doc_with
             [ "aaaaaaaaaaaaaaaa\tbbbbbbbbbbbbbbbb\tdemo01\tsource\t.\t.\t.\t.\t.\t1..30";
               "bbbbbbbbbbbbbbbb\taaaaaaaaaaaaaaaa\tdemo01\tsource\t.\t.\t.\t.\t.\t1..30" ] []));
    Testing.check_raises ~re:".*attributes table names feature.*"
      "an attributes row attaching to no feature is refused"
      (fun () ->
        A.Tabular.of_string
          (doc_with
             [ "aaaaaaaaaaaaaaaa\t.\tdemo01\tsource\t.\t.\t.\t.\t.\t1..30" ]
             [ "cccccccccccccccc\tnote\torphan" ]));
    Testing.check_raises ~re:".*has no .* section.*"
      "a document missing a section is refused"
      (fun () -> A.Tabular.of_string "#!metadata\nkey\tvalue\n");
    (* The feature's own identifier is not always derivable from an attribute --
       the GenBank reader names a record's source feature after its LOCUS -- so
       it needs a column of its own.  Without one it was silently dropped, and
       the second render disagreed with the first. *)
    Testing.check_string "a feature id that is in no attribute still survives"
      ~expected:"demo01"
      (let back = round_trip gb in
       match List.nth_opt (features back) 0 with
       | Some (_, f) -> Option.value ~default:"(none)" f.A.Annotation.id
       | None -> "(no features)"))

(* The NCBI submission feature table. *)

let test_feature_table () =
  Testing.section "NCBI feature table" (fun () ->
    let tbl feature_lines = genbank feature_lines |> A.GenBank.of_string |> A.Tbl.to_string in
    let lines_of s =
      String.Split.on_char_as_list '\n' s |> List.filter (fun l -> l <> "") in
    let coordinate_lines s =
      lines_of s |> List.filter (fun l -> String.length l > 0 && l.[0] <> '\t' && l.[0] <> '>') in
    Testing.check_string "the block header names the sequence"
      ~expected:">Feature demo01"
      (match lines_of (tbl [ "     CDS             1..15" ]) with
       | first :: _ -> first
       | [] -> "(empty)");
    Testing.check_string "a forward feature is written low then high"
      ~expected:"1\t15\tCDS"
      (match coordinate_lines (tbl [ "     CDS             1..15" ]) with
       | _source :: cds :: _ -> cds
       | _ -> "(missing)");
    (* There is no strand column: the minus strand IS the inverted range. *)
    Testing.check_string "a reverse feature is written high then low"
      ~expected:"30\t16\tCDS"
      (match coordinate_lines (tbl [ "     CDS             complement(16..30)" ]) with
       | _source :: cds :: _ -> cds
       | _ -> "(missing)");
    Testing.check_string "extra intervals are bare coordinate lines"
      ~expected:"1\t9\tCDS|20\t28"
      (match coordinate_lines (tbl [ "     CDS             join(1..9,20..28)" ]) with
       | _source :: a :: b :: _ -> a ^ "|" ^ b
       | _ -> "(missing)");
    (* Intervals run 5' to 3' along the FEATURE, so a minus-strand join lists
       them in the reverse of the genomic order they are stored in. *)
    Testing.check_string "a reverse join lists its intervals in feature order"
      ~expected:"28\t20\tCDS|9\t1"
      (match coordinate_lines (tbl [ "     CDS             complement(join(1..9,20..28))" ]) with
       | _source :: a :: b :: _ -> a ^ "|" ^ b
       | _ -> "(missing)");
    Testing.check "a qualifier line carries three empty leading columns"
      (fun () ->
        lines_of (tbl [ "     CDS             1..15"; "                     /gene=\"abcA\"" ])
        |> List.exists (fun l -> l = "\t\t\tgene\tabcA"));
    (* A valueless qualifier simply omits the fifth column, which is how a
       feature table says "present, no value". *)
    Testing.check "a valueless qualifier omits its value column"
      (fun () ->
        lines_of (tbl [ "     CDS             1..15"; "                     /pseudo=\"\"" ])
        |> List.exists (fun l -> l = "\t\t\tpseudo"));
    (* /codon_start is the only slot a feature table has for a phase, so a phase
       that arrived in a GFF3 column has to be rendered as one. *)
    Testing.check "a phase with no /codon_start is written as one"
      (fun () ->
        let from_gff3 =
          gff3 [ "chr1\tdemo\tgene\t100\t500\t.\t+\t.\tID=g1";
                 "chr1\tdemo\tmRNA\t100\t500\t.\t+\t.\tID=t1;Parent=g1";
                 "chr1\tdemo\tCDS\t100\t500\t.\t+\t2\tID=c1;Parent=t1" ]
          |> A.GFF3.of_string |> A.Tbl.to_string in
        lines_of from_gff3 |> List.exists (fun l -> l = "\t\t\tcodon_start\t3"));
    (* A zero-length site has no spelling here at all, and quietly widening it
       to one base would be a submission that says something the annotation did
       not. *)
    Testing.check_raises ~re:".*zero-length.*"
      "a zero-length feature is refused rather than widened"
      (fun () -> tbl [ "     misc_feature    10^11" ]))

(* Insertion invariants.  These are not defects -- they are the contract a
   tabular reader has to satisfy, pinned so that a change to it is deliberate. *)

let test_add_invariants () =
  Testing.section "Annotation.add invariants" (fun () ->
    let h = A.Hierarchy.of_string "(gene ((mRNA (exon))))" in
    let ann = A.Annotation.create h in
    let feature seq =
      { A.Annotation.empty_feature with
        A.Annotation.seq = A.Annotation.intern_seq ann seq;
        intervals = [ { T.low = 0; length = 10 } ] } in
    Testing.check_string "a hierarchy round-trips through to_string"
      ~expected:"(gene ((mRNA (exon))))" (A.Hierarchy.to_string h);
    (* The DFS-ordered-insertion invariant: an internal path segment must already
       exist as the most recent node at the previous level.  This is exactly why
       a filtered tabular file cannot be read back without either synthesising
       the missing parents or refusing the input. *)
    Testing.check_raises ~re:".*No parent of category.*"
      "adding a child whose parent is absent raises"
      (fun () -> A.Annotation.add ann ~path:[ "annotation"; "gene"; "mRNA" ] (feature "chr1"));
    Testing.check_raises ~re:".*not valid against hierarchy.*"
      "adding a feature at a path outside the hierarchy raises"
      (fun () -> A.Annotation.add ann ~path:[ "annotation"; "nonesuch" ] (feature "chr1"));
    Testing.check_does_not_raise "adding a root-level feature succeeds"
      (fun () -> A.Annotation.add ann ~path:[ "annotation"; "gene" ] (feature "chr1")))

(* Insertion cost.  Not a benchmark: a guard on the ASYMPTOTICS.  Sibling lists
   are held most-recent-first precisely so that Annotation.add is O(1) at each
   level; restore the append-and-walk-to-the-end version and every functional
   check here still passes while a flat million-feature file goes from seconds
   to hours.  The threshold is deliberately loose -- linear predicts about 4,
   quadratic about 16 -- so that a loaded machine cannot make it flap. *)

let test_insertion_cost () =
  Testing.section "Insertion cost" (fun () ->
    let h = A.Hierarchy.of_string "gene" in
    let build n =
      let ann = ref (A.Annotation.create h) in
      let seq = A.Annotation.intern_seq !ann "chr1" in
      let started = Unix.gettimeofday () in
      for i = 0 to n - 1 do
        ann :=
          A.Annotation.add !ann ~path:[ "annotation"; "gene" ]
            { A.Annotation.empty_feature with
              A.Annotation.seq;
              intervals = [ { T.low = i * 100; length = 50 } ] }
      done;
      Unix.gettimeofday () -. started in
    (* Large enough that both measurements sit well clear of scheduler noise --
       at a few thousand features the smaller one lands near a millisecond and
       the ratio is mostly jitter. *)
    let _warm = build 5000 in
    let small = build 20000 in
    let large = build 80000 in
    let ratio = large /. Float.max small 1e-3 in
    Testing.check
      (Printf.sprintf
         "quadrupling the feature count costs far less than quadratically \
          (%.0f ms -> %.0f ms, %.1fx)" (small *. 1000.) (large *. 1000.) ratio)
      (fun () -> ratio < 8.);
    (* Order still has to be insertion order, which is the property the reversed
       representation is quietly relying on. *)
    Testing.check_string "features come back in insertion order"
      ~expected:"0,100,200,300,400"
      (let ann = ref (A.Annotation.create h) in
       let seq = A.Annotation.intern_seq !ann "chr1" in
       for i = 0 to 4 do
         ann :=
           A.Annotation.add !ann ~path:[ "annotation"; "gene" ]
             { A.Annotation.empty_feature with
               A.Annotation.seq;
               intervals = [ { T.low = i * 100; length = 50 } ] }
       done;
       let acc = ref [] in
       A.Annotation.iter (fun ~path:_ f ->
         match f.A.Annotation.intervals with
         | i :: _ -> List.accum acc (string_of_int i.T.low)
         | [] -> ()) !ann;
       List.rev !acc |> String.concat ","))

(* Sequence handling that AnnoTools extraction depends on.  These live in
   Sequences rather than Annotations, but they decide whether --extract-dna
   would return the right bases, so they are pinned here until that action
   exists and can own them. *)

let test_extraction_prerequisites () =
  Testing.section "Extraction prerequisites (Sequences)" (fun () ->
    Testing.check_string "reverse complement of an unambiguous sequence"
      ~expected:"ACCGGGCAT" (Sequences.Lint.rc "ATGCCCGGT");
    (* dnaize folds every non-ACGT byte to N.  AnnoTools.ml:342 loads the
       reference with this default, so an IUPAC-containing reference is N-masked
       before any feature sequence is ever extracted from it. *)
    Testing.check_string "the default linter folds IUPAC codes to N"
      ~expected:"ACGTNNNN"
      (Sequences.Lint.dnaize ~keep_lowercase:false ~keep_dashes:false "acgtrykm");
    (* ... and rc must complement the ambiguity codes too, or relaxing the linter
       would turn honest Ns into WRONG bases on the minus strand: R (A/G) has to
       become Y (C/T), not stay R.  The two therefore had to be fixed together. *)
    Testing.check_string "reverse complement handles IUPAC ambiguity codes"
      ~expected:"YR" (Sequences.Lint.rc "YR");
    (* Reversed, then complemented pairwise: N D H V B K M R Y W S U T G C A
       becomes N H D B V M K Y R W S A A C G T.  U complements to A, which is
       why it is excluded from the involution check below. *)
    Testing.check_string "every IUPAC code has a complement"
      ~expected:"NHDBVMKYRWSAACGT"
      (Sequences.Lint.rc "ACGTUSWYRMKBVHDN");
    (* Complementing is an involution, so rc . rc is the identity.  This is the
       check that would catch a table entry that maps two codes to the same
       complement. *)
    let iupac = "ACGTSWYRMKBVHDNacgtswyrmkbvhdn" in
    Testing.check_string "reverse complement is an involution"
      ~expected:iupac (Sequences.Lint.rc (Sequences.Lint.rc iupac));
    Testing.check_string "case is preserved by reverse complement"
      ~expected:"yrYR" (Sequences.Lint.rc "YRyr");
    (* Gaps and anything outside the nucleotide alphabet are carried through, so
       an aligned sequence can be reverse-complemented without losing its
       columns. *)
    Testing.check_string "gaps survive reverse complement"
      ~expected:"C-G" (Sequences.Lint.rc "C-G"))

let run () =
  test_locations ();
  test_genbank_records ();
  test_genbank_round_trip ();
  test_feature_sequence ();
  test_selection ();
  test_attributes ();
  test_gff3_fidelity ();
  test_attribute_order ();
  test_tabular ();
  test_feature_table ();
  test_add_invariants ();
  test_insertion_cost ();
  test_extraction_prerequisites ()
