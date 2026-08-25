(*
    Annotations_GenBank.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Annotations_GenBank.ml reads and writes GenBank flat files: the
    record framing, the FEATURES block with its multi-line LOCATION
    expressions and [/qualifier=] sub-language, and the ORIGIN block
    from which a reference is built.  The LOCATION grammar itself
    lives on [Annotations_Common.GenBankLocation].

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

open Better
open Annotations_Common
(* The base AST, unqualified, as every format reader and writer uses it. *)
open Annotation

(* GenBank satisfies [Format_t] and additionally exposes the
   record-list parser, which is useful to callers that want to
   inspect raw GenBank input without going through the full
   [Annotation.t] construction.  The LOCATION-string entry
   point lives on the extended [GenBankLocation] module
   ([GenBankLocation.of_string]). *)
module GenBank:
  sig
    include Format_t
    val parse_records: string -> GenBankRecord.t list
  end
= struct
  (* INSDC feature table standard categories, flat under an
     implicit [source]: features in a GenBank record do not
     carry explicit parent links and the format groups them
     only by interval containment.  Users wanting a richer
     hierarchy can swap this out via [?hierarchy]. *)
  let default_hierarchy =
    Hierarchy.of_string
      "(source \
         (gene, mRNA, tRNA, rRNA, ncRNA, misc_RNA, \
          CDS, exon, intron, \
          five_prime_UTR, three_prime_UTR, \
          promoter, regulatory, \
          repeat_region, misc_feature, \
          variation, polyA_signal, polyA_site, \
          primer_bind, protein_bind, \
          oriT, oriC, \
          sig_peptide, mat_peptide, propeptide, \
          stem_loop, terminator))"
  let dialects = [ "standard", default_hierarchy ]
  (* GenBank record framing is handled by the modal [genbank]
     ocamllex rule: it reads the file line by line, classifying
     each non-blank line into one of nine token classes against
     a [Headers | Features | Origin] mode reset on every [//]
     boundary.  The Menhir grammar [genbank_records] then
     stitches feature continuations onto their location and
     qualifier continuations onto their value, returning a list
     of [GenBankRecord.t] records that the driver below lifts
     into the [Annotation.t] AST. *)
  let parse_records s =
    Annotations_Lex.reset_genbank_mode ();
    let lexbuf = Lexing.from_string ~with_positions:true s in
    Annotations_Parse.genbank_records Annotations_Lex.genbank lexbuf
  (* Pull (locus, sequence-length) out of the LOCUS header
     line.  Standard form is "NAME LEN bp ..." where the [bp]
     marker follows the integer length; we tolerate extra
     whitespace and stop at the first integer-then-bp pair. *)
  let locus_and_length headers =
    match List.assoc_opt "LOCUS" headers with
    | None -> "", 0
    | Some v ->
      let parts =
        String.split_on_char ' ' v |> List.filter (fun x -> x <> "") in
      (match parts with
       | name :: rest ->
         let rec find_len = function
           | [] -> 0
           | a :: "bp" :: _ -> (try int_of_string a with _ -> 0)
           | _ :: rest -> find_len rest in
         name, find_len rest
       | [] -> "", 0)
  (* Convert one parsed [GenBankRecord.feature_t] to
     (path, feature) under the active hierarchy, interning
     [seq_name] and attribute keys into the supplied tables.
     Multi-interval (join/order) features fold to a single
     feature with multiple intervals; the strand carries
     through. *)
  (* Aggregate qualifier repeats into per-key lists, and freeze them into
     [Attributes] over interned values.  Used both by [feature_to_pair] and by
     the source-feature synthesis in [read] below.
     The qualifiers are walked ONCE, in the order the file wrote them: a key
     already seen has its value appended and keeps the position it had, and a
     new key goes last.  Aggregating into a StringMap first and folding that,
     as this did, gathered the repeats correctly and then handed the keys back
     in ALPHABETICAL order -- so a feature whose file says /product before
     /gene came out with them the other way round, and no format could put it
     right because the order was gone by then. *)
  let attrs_of_qualifiers ~attr_keys ~values qualifiers =
    let acc_lists = ref StringMap.empty and attrs = ref Attributes.empty in
    List.iter
      (fun (k, v) ->
        let vs = (try StringMap.find k !acc_lists with Not_found -> []) @ [ v ] in
        acc_lists := StringMap.add k vs !acc_lists;
        let kid = AttrKey.intern attr_keys k
        and arr = Array.of_list (List.map (ValueTable.intern values) vs) in
        attrs := Attributes.add kid arr !attrs)
      qualifiers;
    !acc_lists, !attrs
  let feature_to_pair ~seqs ~attr_keys ~values
                      hierarchy seq_name
                      (f : GenBankRecord.feature_t) =
    let ftype = f.name in
    let path = [ implicit_root_name; "source"; ftype ] in
    if not (Hierarchy.validate hierarchy ~path) then
      Exception.raise __FUNCTION__ IO_Format
        (Printf.sprintf
           "GenBank: feature %S not allowed under hierarchy %s"
           ftype (Hierarchy.to_string hierarchy));
    let loc = GenBankLocation.of_string f.location in
    let pieces, strand = GenBankLocation.intervals loc in
    let intervals = List.map (fun (_, ivl) -> ivl) pieces in
    let acc_lists, attrs =
      attrs_of_qualifiers ~attr_keys ~values f.qualifiers in
    let lookup_str_attr name =
      match StringMap.find_opt name acc_lists with
      | Some (v :: _) -> Some v
      | _ -> None in
    let id =
      match lookup_str_attr "locus_tag" with
      | Some _ as r -> r
      | None -> lookup_str_attr "gene" in
    (* /codon_start is GenBank's spelling of GFF3's phase, counted from 1 rather
       than from 0, so phase = codon_start - 1.  A CDS carrying no /codon_start
       starts in frame, which is phase 0; leaving it None here made every
       out-of-frame CDS fail validate_translation for the wrong reason. *)
    let phase =
      match lookup_str_attr "codon_start" with
      | None -> None
      | Some s ->
        (match int_of_string_opt (String.trim s) with
         | Some n when n >= 1 && n <= 3 -> Some (n - 1)
         | _ ->
           Exception.raise __FUNCTION__ IO_Format
             (Printf.sprintf "GenBank: invalid /codon_start %S on feature %S (expected 1, 2 or 3)"
                s ftype)) in
    let feature = {
      seq = Seq.intern seqs seq_name;
      source = None;
      intervals;
      (* GenBank has no score column: the concept is GFF3's. *)
      score = None;
      strand;
      phase;
      id;
      attributes = attrs
    } in
    (path, feature)
  (* Carrier-based reader: install the GenBank features
     encoded in [s] into [ann_in], using the carrier's
     hierarchy for validation.  Each record's headers go to the
     carrier's metadata (minus the redundant LOCUS line, which
     the writer regenerates).  When at least one record carries
     an ORIGIN block, the resulting reference -- accumulated
     across all such records -- replaces whatever reference was
     previously attached to the carrier. *)
  let read ann_in s =
    let records = parse_records s in
    if records = [] then ann_in
    else
      let ann = ref ann_in in
      let hierarchy = Annotation.hierarchy !ann in
      let ref_acc = ref Sequences.Reference.empty in
      let any_origin = ref false in
      let first_record = ref true in
      List.iter (fun (r : GenBankRecord.t) ->
        (* Each GenBank record is one sequence: drop the
           Bloom on every record boundary except the first
           (where it's already empty). *)
        if !first_record then first_record := false
        else ValueTable.drop_bloom (values !ann);
        let locus, seq_length = locus_and_length r.headers in
        let source_path = [ implicit_root_name; "source" ] in
        let real_source =
          List.find_opt
            (fun (gf : GenBankRecord.feature_t) ->
               gf.name = "source") r.features in
        if Hierarchy.validate hierarchy ~path:source_path then begin
          (* Build the top-level source feature from the file's
             own [source] feature if there is one (so its
             /organism, /mol_type, /isolate, /host, /db_xref,
             ... qualifiers carry over to the AST), and fall
             back to a bare LOCUS-line-derived span otherwise. *)
          let source_feature =
            match real_source with
            | Some f ->
              let loc = GenBankLocation.of_string f.location in
              let pieces, _ = GenBankLocation.intervals loc in
              let intervals = List.map (fun (_, ivl) -> ivl) pieces in
              let _, attrs =
                attrs_of_qualifiers
                  ~attr_keys:(attr_keys !ann)
                  ~values:(values !ann) f.qualifiers in
              { seq = Seq.intern (seqs !ann) locus;
                source = None;
                intervals;
                score = None;
                strand = None;
                phase = None;
                id = Some locus;
                attributes = attrs }
            | None ->
              let source_iv : Sequences.Types.simple_interval_t = {
                low = 0;
                length = seq_length
              } in
              { seq = Seq.intern (seqs !ann) locus;
                source = None;
                intervals = [ source_iv ];
                score = None;
                strand = None;
                phase = None;
                id = Some locus;
                attributes = Attributes.empty } in
          ann := add !ann ~path:source_path source_feature
        end;
        (* The LOCUS line is regenerated canonically by the
           writer from [seq] and the feature span, so we
           don't echo it through metadata -- otherwise the
           output would carry two LOCUS lines. *)
        List.iter (fun (k, v) ->
          if k <> "LOCUS" then
            ann := add_metadata !ann ~key:k ~value:v
        ) r.headers;
        List.iter (fun (gf : GenBankRecord.feature_t) ->
          if gf.name <> "source" then
            let path, feature =
              feature_to_pair
                ~seqs:(seqs !ann) ~attr_keys:(attr_keys !ann)
                ~values:(values !ann)
                hierarchy locus gf in
            ann := add !ann ~path feature
        ) r.features;
        (match r.origin with
         | None -> ()
         | Some seq ->
           any_origin := true;
           (* The bytes are already in memory, so they are handed straight to
              the loader.  This used to go out to a temporary file and come
              back, which was the only way in before
              [add_from_fasta_string] existed: a write and a delete per record,
              failing outright wherever the temporary directory is not
              writable, and leaving the file behind if the process died between
              the two.
              Upper-cased and otherwise left alone, as for the other two ways a
              reference can arrive; see the note in the GFF3 reader.  The
              default linter that the detour used to apply folded every
              ambiguity code in an ORIGIN block to N. *)
           ref_acc :=
             Sequences.Reference.add_from_fasta_string ~linter:String.uppercase_ascii
               !ref_acc (Printf.sprintf ">%s\n%s\n" locus seq))
      ) records;
      cleanup_values !ann;
      if !any_origin then set_reference !ann !ref_acc else !ann
  let read_from_file ann path = read ann (read_file path)
  let of_string ?(hierarchy = default_hierarchy) s =
    read (create hierarchy) s
  let of_file ?(hierarchy = default_hierarchy) path =
    read_from_file (create hierarchy) path
  (* Output: minimal GenBank with a FEATURES section.  Uses
     each feature's [seq] as the LOCUS name and emits the
     standard 21-column qualifier indentation.  Locations
     are reconstructed from [intervals] and [strand].  ORIGIN
     is emitted when a [Sequences.Reference] is attached. *)
  let format_intervals_strand intervals strand =
    let parts =
      (* [OneBased] carries the zero-length case: a site between two bases is
         INSDC's [lo^hi], and running it through the ordinary formula would
         emit the reversed range [low+1..low] instead, which this reader
         rejects and no other tool would accept either. *)
      List.map (fun i -> OneBased.(of_interval i |> to_string)) intervals in
    let body =
      match parts with
      | [] -> "1"
      | [ p ] -> p
      | _ ->
        Printf.sprintf "join(%s)" (String.concat "," parts) in
    match strand with
    | Some Sequences.Types.Reverse _ ->
      Printf.sprintf "complement(%s)" body
    | _ -> body
  let to_buffer buf ann =
    (* Group features by [seq], preserving source order.
       Each feature is paired with its leaf category so we
       can emit the right name on the FEATURES line. *)
    let by_seq = Hashtbl.create 16 in
    let order = ref [] in
    iter_paths (fun ~path feature ->
      let seq = seq_name ann feature in
      if not (Hashtbl.mem by_seq seq) then
        List.accum order seq;
      let prev =
        try Hashtbl.find by_seq seq with Not_found -> [] in
      let leaf =
        match List.rev path with x :: _ -> x | [] -> "" in
      Hashtbl.replace by_seq seq ((leaf, feature) :: prev)
    ) ann;
    let order = List.rev !order in
    List.iter (fun seq ->
      let feats = List.rev
        (try Hashtbl.find by_seq seq with _ -> []) in
      let total_len =
        List.fold_left (fun acc (_, f) ->
          List.fold_left
            (fun acc (i : Sequences.Types.simple_interval_t) ->
              max acc (i.low + i.length)) acc f.intervals
        ) 0 feats in
      Printf.bprintf buf "LOCUS       %-16s%d bp    DNA\n"
        seq total_len;
      (* A sub-keyword sits in column 3 with its value in column 13, and has to
         follow the keyword it belongs to.  The metadata map is ordered by key,
         so ORGANISM would otherwise come out at column 1 and in alphabetical
         position -- which is not GenBank, and is not what was read in. *)
      let subs_of = function
        | "SOURCE" -> [ "ORGANISM" ]
        | "REFERENCE" -> [ "AUTHORS"; "CONSRTM"; "TITLE"; "JOURNAL"; "PUBMED"; "REMARK" ]
        | _ -> [] in
      let metadata = all_metadata ann in
      let every_sub = List.concat_map subs_of [ "SOURCE"; "REFERENCE" ] in
      let emit ~indented k =
        match StringMap.find_opt k metadata with
        | None -> ()
        | Some vs ->
          List.iter (fun v ->
            if indented then Printf.bprintf buf "  %-10s%s\n" k v
            else Printf.bprintf buf "%-12s%s\n" k v) vs in
      StringMap.iter (fun k _ ->
        if not (List.mem k every_sub) then begin
          emit ~indented:false k;
          List.iter (emit ~indented:true) (subs_of k)
        end) metadata;
      Buffer.add_string buf
        "FEATURES             Location/Qualifiers\n";
      List.iter (fun (leaf, f) ->
        let name = if leaf = "" then "misc_feature" else leaf in
        let location =
          format_intervals_strand f.intervals f.strand in
        Printf.bprintf buf "     %-16s%s\n" name location;
        attr_iter ann (fun k vs ->
          List.iter (fun v ->
            Printf.bprintf buf
              "                     /%s=%S\n" k v
          ) vs
        ) f
      ) feats;
      (match reference ann with
       | Some r ->
         let opt_seq =
           try
             Some (fst (Sequences.Reference.find r
                          (Sequences.Types.Forward seq)))
           with _ -> None in
         (match opt_seq with
          | None -> ()
          | Some s ->
            Buffer.add_string buf "ORIGIN\n";
            let n = String.length s in
            let i = ref 0 in
            while !i < n do
              Printf.bprintf buf "%9d" (!i + 1);
              let row_end = min n (!i + 60) in
              let j = ref !i in
              while !j < row_end do
                if (!j - !i) mod 10 = 0 then
                  Buffer.add_char buf ' ';
                Buffer.add_char buf
                  (Char.lowercase_ascii s.[!j]);
                incr j
              done;
              Buffer.add_char buf '\n';
              i := row_end
            done)
       | None -> ());
      Buffer.add_string buf "//\n"
    ) order
  let to_string = to_string_via_buffer to_buffer
  let to_file = to_file_via_buffer to_buffer
end

