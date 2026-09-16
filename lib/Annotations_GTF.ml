(*
    Annotations_GTF.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Annotations_GTF.ml reads and writes GTF, the Ensembl/GENCODE
    flavour whose hierarchy is implicit in the [gene_id] and
    [transcript_id] attributes rather than in a [Parent] link.  Plain
    UCSC GTF ships only leaf rows, so the reader synthesises the
    missing gene and transcript parents from the union of their
    children's intervals.

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

(* GTF satisfies [Format_t] without any per-module extensions. *)
module GTF: Format_t = struct
  let default_hierarchy =
    Hierarchy.of_string
      "(gene \
         ((transcript \
            (exon, CDS, five_prime_utr, three_prime_utr, \
             start_codon, stop_codon, Selenocysteine))))"
  let dialects = [ "standard", default_hierarchy ]
  let parse_attributes s =
    let lexbuf = Lexing.from_string ~with_positions:true s in
    parse_with ~what:(Printf.sprintf "GTF attributes %S" s)
      Annotations_Parse.gtf_attribute_list Annotations_Lex.gtf_attributes lexbuf
  type row_t = {
    gtf_seq: string;
    gtf_source: string;
    gtf_type: string;
    gtf_lo: int;
    gtf_hi: int;
    gtf_score: float option;
    gtf_strand: Sequences.Types.strand_t option;
    gtf_phase: int option;
    gtf_attrs: string list StringMap.t;
    gtf_gene_id: string option;
    gtf_tx_id: string option;
    gtf_lnum: int
  }
  let parse_row line_no fields =
    if Array.length fields <> 9 then
      Exception.raise __FUNCTION__ IO_Format
        (Printf.sprintf
           "On line %d: GTF row has %d columns, expected 9"
           line_no (Array.length fields));
    let attrs = parse_attributes fields.(8) in
    let attr_map =
      List.fold_left (fun m (k, vs) ->
        let prev = try StringMap.find k m with Not_found -> [] in
        StringMap.add k (prev @ vs) m
      ) StringMap.empty attrs in
    let lookup1 k =
      match StringMap.find_opt k attr_map with
      | Some (v :: _) -> Some v
      | _ -> None in
    {
      gtf_seq = fields.(0);
      gtf_source = fields.(1);
      gtf_type = fields.(2);
      gtf_lo = int_of_string fields.(3);
      gtf_hi = int_of_string fields.(4);
      gtf_score = score_of_field fields.(5);
      gtf_strand = strand_of_field fields.(6);
      gtf_phase = phase_of_field fields.(7);
      gtf_attrs = attr_map;
      gtf_gene_id = lookup1 "gene_id";
      gtf_tx_id = lookup1 "transcript_id";
      gtf_lnum = line_no
    }
  let read_rows s =
    let rows = ref [] in
    iter_tsv_lines s
      ~data:(fun lnum fields ->
        List.accum rows (parse_row lnum fields));
    List.rev !rows
  let value_array_of_strings values vs =
    Array.of_list (List.map (ValueTable.intern values) vs)
  let intern_source_field values s =
    if s = "" || s = "." then None
    else Some (ValueTable.intern values s)
  (* Build a feature_t from a [row_t], interning into the
     supplied tables. *)
  let feature_of_row ~seqs ~attr_keys ~values r =
    let attr_map =
      StringMap.fold (fun k vs m ->
        let kid = AttrKey.intern attr_keys k in
        Attributes.add kid (value_array_of_strings values vs) m
      ) r.gtf_attrs Attributes.empty in
    {
      seq = Seq.intern seqs r.gtf_seq;
      source = intern_source_field values r.gtf_source;
      intervals =
        [ Segment.make (interval_of_1_based ~lo:r.gtf_lo ~hi:r.gtf_hi) ];
      score = r.gtf_score;
      strand = r.gtf_strand;
      phase = r.gtf_phase;
      id = None;
      attributes = attr_map
    }
  let synth_feature ~seqs ~attr_keys ~values ~seq ~source
                    ~lo ~hi ~strand ~id ~attrs =
    let attr_map =
      List.fold_left (fun m (k, vs) ->
        let kid = AttrKey.intern attr_keys k in
        Attributes.add kid (value_array_of_strings values vs) m
      ) Attributes.empty attrs in
    {
      seq = Seq.intern seqs seq;
      source = intern_source_field values source;
      intervals = [ Segment.make (interval_of_1_based ~lo ~hi) ];
      (* A synthesised parent has no score of its own: GTF gives one only to
         the rows actually present in the file. *)
      score = None;
      strand;
      phase = None;
      id = Some id;
      attributes = attr_map
    }
  (* GTF normally has explicit gene/transcript rows from
     Ensembl/GENCODE, but plain GTF (UCSC, pure-CDS) often
     only has exon/CDS rows.  We synthesise gene/transcript
     spans from the union of children intervals when they
     are not present in the source. *)
  let gtf_walk_dfs ~seqs ~attr_keys ~values hierarchy rows =
    let span_of_intervals ivs =
      List.fold_left (fun (lo, hi) (i, j) ->
        (min lo i), (max hi j)
      ) (max_int, min_int) ivs in
    (* Group by (seq, gene_id) and within each by tx_id. *)
    let by_gene = Hashtbl.create 16 in
    let by_tx = Hashtbl.create 64 in
    let gene_order = ref [] in
    let tx_order = ref [] in
    let gene_explicit = Hashtbl.create 16 in
    let tx_explicit = Hashtbl.create 64 in
    List.iter (fun r ->
      let gid =
        match r.gtf_gene_id with
        | Some s -> s
        | None ->
          Exception.raise __FUNCTION__ IO_Format
            (Printf.sprintf
               "On line %d: GTF row missing gene_id"
               r.gtf_lnum) in
      let key_g = r.gtf_seq, gid in
      if not (Hashtbl.mem by_gene key_g) then begin
        Hashtbl.add by_gene key_g [];
        List.accum gene_order key_g
      end;
      (match r.gtf_type with
       | "gene" -> Hashtbl.replace gene_explicit key_g r
       | "transcript" ->
         let tid =
           match r.gtf_tx_id with Some s -> s | None ->
             Exception.raise __FUNCTION__ IO_Format
               (Printf.sprintf
                  "On line %d: transcript row missing transcript_id"
                  r.gtf_lnum) in
         let key_t = r.gtf_seq, gid, tid in
         (* Register the transcript in its own right, and not only when some
            child row happens to mention it.  [tx_order] is what the emit loop
            below walks, so a transcript declared with no exons -- legal GTF,
            and the shape a gene with a single unspliced product takes -- was
            otherwise dropped without a word. *)
         if not (Hashtbl.mem by_tx key_t) then begin
           Hashtbl.add by_tx key_t [];
           List.accum tx_order key_t
         end;
         Hashtbl.replace tx_explicit key_t r
       | _ ->
         match r.gtf_tx_id with
         | None ->
           (* GTF wants a transcript_id on every row but a gene's.  A row without
              one used to be dropped here without a word, and its feature with it. *)
           Exception.raise __FUNCTION__ IO_Format
             (Printf.sprintf "On line %d: GTF %s row missing transcript_id"
                r.gtf_lnum r.gtf_type)
         | Some tid ->
           let key_t = r.gtf_seq, gid, tid in
           if not (Hashtbl.mem by_tx key_t) then begin
             Hashtbl.add by_tx key_t [];
             List.accum tx_order key_t
           end;
           Hashtbl.replace by_tx key_t
             ((Hashtbl.find by_tx key_t) @ [r]))
    ) rows;
    let gene_order = List.rev !gene_order in
    let tx_order = List.rev !tx_order in
    let acc = ref [] in
    let emit (path: string list) (feature: feature_t) =
      if not (Hierarchy.validate hierarchy ~path) then
        Exception.raise __FUNCTION__ IO_Format
          (Printf.sprintf
             "GTF: path %s violates hierarchy %s"
             (path_to_string path)
             (Hierarchy.to_string hierarchy));
      List.accum acc (path, feature) in
    List.iter (fun (seq, gid) ->
      let key_g = (seq, gid) in
      let gene_row =
        try Some (Hashtbl.find gene_explicit key_g)
        with Not_found -> None in
      (* gather all transcript keys belonging to this gene,
         in source order. *)
      let txs =
        List.filter (fun (s, g, _) -> s = seq && g = gid)
          tx_order in
      (* compute gene span as union of all sub-features. *)
      let gene_span =
        let collected = ref [] in
        List.iter (fun key_t ->
          List.iter (fun r ->
            List.accum collected (r.gtf_lo, r.gtf_hi)
          ) (try Hashtbl.find by_tx key_t with _ -> [])
        ) txs;
        (match gene_row with
         | Some r -> List.accum collected (r.gtf_lo, r.gtf_hi)
         | None -> ());
        if !collected = [] then 1, 1
        else span_of_intervals !collected in
      let gene_lo, gene_hi = gene_span in
      let strand_of_first =
        match gene_row with
        | Some r -> r.gtf_strand
        | None ->
          (match txs with
           | (_, _, _) as kt :: _ ->
             (match Hashtbl.find by_tx kt with
              | r :: _ -> r.gtf_strand
              | [] -> None)
           | [] -> None) in
      let gene_feature =
        match gene_row with
        | Some r ->
          let f = feature_of_row ~seqs ~attr_keys ~values r in
          { f with id = Some gid }
        | None ->
          synth_feature ~seqs ~attr_keys ~values
            ~seq ~source:"" ~lo:gene_lo ~hi:gene_hi
            ~strand:strand_of_first ~id:gid
            ~attrs:[ "gene_id", [ gid ] ] in
      emit [ implicit_root_name; "gene" ] gene_feature;
      List.iter (fun key_t ->
        let _, _, tid = key_t in
        let tx_row =
          try Some (Hashtbl.find tx_explicit key_t)
          with Not_found -> None in
        let kids =
          try Hashtbl.find by_tx key_t with _ -> [] in
        let tx_span =
          let pts = List.map (fun r ->
            r.gtf_lo, r.gtf_hi) kids in
          let pts =
            match tx_row with
            | Some r -> (r.gtf_lo, r.gtf_hi) :: pts
            | None -> pts in
          if pts = [] then 1, 1 else span_of_intervals pts in
        let tx_lo, tx_hi = tx_span in
        let tx_strand =
          match tx_row, kids with
          | Some r, _ -> r.gtf_strand
          | None, r :: _ -> r.gtf_strand
          | _ -> strand_of_first in
        let tx_feature =
          match tx_row with
          | Some r ->
            let f = feature_of_row ~seqs ~attr_keys ~values r in
            { f with id = Some tid }
          | None ->
            synth_feature ~seqs ~attr_keys ~values
              ~seq ~source:"" ~lo:tx_lo ~hi:tx_hi
              ~strand:tx_strand ~id:tid
              ~attrs:[ "gene_id", [ gid ];
                       "transcript_id", [ tid ] ] in
        emit [ implicit_root_name; "gene"; "transcript" ]
          tx_feature;
        List.iter (fun r ->
          let leaf = r.gtf_type in
          let path =
            [ implicit_root_name; "gene"; "transcript"; leaf ] in
          let feature = feature_of_row ~seqs ~attr_keys ~values r in
          emit path feature
        ) kids
      ) txs
    ) gene_order;
    List.rev !acc
  (* Carrier-based reader: install the GTF features encoded in
     [s] into [ann_in], using the carrier's hierarchy for
     validation and gene/transcript synthesis. *)
  let read ann_in s =
    let ann = ref ann_in in
    let hierarchy = Annotation.hierarchy !ann in
    let rows = read_rows s in
    let dfs =
      gtf_walk_dfs
        ~seqs:(seqs !ann) ~attr_keys:(attr_keys !ann)
        ~values:(values !ann)
        hierarchy rows in
    add_dfs_with_seq_bloom ann dfs;
    cleanup_values !ann;
    !ann
  let read_from_file ann path = read ann (read_file path)
  let of_string ?(hierarchy = default_hierarchy) s =
    read (create hierarchy) s
  let of_file ?(hierarchy = default_hierarchy) path =
    read_from_file (create hierarchy) path
  (* [gene_id] and [transcript_id] lead, as GTF wants them on every row, and what
     else the feature carries follows -- less those two, which they restate, and
     less GFF3's [ID] and [Parent], which say nothing a GTF reader can use. *)
  let attribute_string ann ~gene_id ?transcript_id feature =
    let own =
      attribute_pairs ann feature
      |> List.filter (fun (k, _) ->
           k <> "ID" && k <> "Parent" && k <> "gene_id" && k <> "transcript_id")
      |> List.concat_map (fun (k, vs) ->
           List.map (fun v -> Printf.sprintf "%s %S" k v) vs) in
    (Printf.sprintf "gene_id %S" gene_id
     :: (match transcript_id with
         | Some t -> [ Printf.sprintf "transcript_id %S" t ]
         | None -> [])
     @ own)
    |> List.map (fun pair -> pair ^ ";")
    |> String.concat " "
  let columns ann feature =
    let strand =
      match feature.strand with
      | Some Sequences.Types.Forward _ -> "+"
      | Some Sequences.Types.Reverse _ -> "-"
      | None -> "."
    and src =
      match feature_source ann feature with
      | Some s -> s | None -> "BiOCamLib" in
    seq_name ann feature, src, strand
  (* One row for each interval of a feature beneath a transcript. *)
  let leaf_rows ann ~category ~attrs feature =
    let seq, src, strand = columns ann feature in
    (* Per ROW, not per feature -- see the GFF3 writer. *)
    let phase_of consumed =
      match feature.phase with
      | None -> "."
      | Some p -> string_of_int (((p - consumed) mod 3 + 3) mod 3) in
    let _, rows =
      List.fold_left (fun (consumed, acc) (s: Segment.t) ->
        let lo, hi = OneBased.bounds s.span in
        consumed + s.span.length,
        Printf.sprintf
          "%s\t%s\t%s\t%d\t%d\t%s\t%s\t%s\t%s"
          seq src category lo hi (field_of_score feature.score) strand (phase_of consumed) attrs
          :: acc)
        (0, []) feature.intervals in
    List.rev rows
  (* The single row of a gene or a transcript, spanning all the feature does. *)
  let span_row ann ~category ~attrs feature =
    let seq, src, strand = columns ann feature in
    let lo, hi =
      List.fold_left (fun (lo, hi) (s: Segment.t) ->
        let l, h = OneBased.bounds s.span in
        min lo l, max hi h)
        (max_int, min_int) feature.intervals in
    Printf.sprintf "%s\t%s\t%s\t%d\t%d\t%s\t%s\t.\t%s"
      seq src category lo hi (field_of_score feature.score) strand attrs
  (* GTF has three levels and no links: a gene row, a transcript row, and the rows
     beneath a transcript, tied together by [gene_id] and [transcript_id].  The
     reader rebuilds exactly [gene -> transcript -> leaf] from them, so this writes
     its inverse by depth: what stands at the first level is a gene, at the second a
     transcript, and anything deeper a row beneath one.  It invents and translates
     nothing.  A register shaped otherwise gets a file shaped like its hierarchy, and
     giving the levels suitable names is the business of whoever chose the
     hierarchy. *)
  let to_buffer buf ann =
    let identifier = identifiers ann in
    let gene = ref "" and transcript = ref "" in
    iter_paths (fun ~path feature ->
      let category = match List.rev path with c :: _ -> c | [] -> "" in
      let rows =
        match List.length path - 1 with
        | 1 ->
          gene := identifier ~category feature.id;
          [ span_row ann ~category
              ~attrs:(attribute_string ann ~gene_id:!gene feature) feature ]
        | 2 ->
          transcript := identifier ~category feature.id;
          [ span_row ann ~category
              ~attrs:(attribute_string ann ~gene_id:!gene ~transcript_id:!transcript feature)
              feature ]
        | _ ->
          leaf_rows ann ~category
            ~attrs:(attribute_string ann ~gene_id:!gene ~transcript_id:!transcript feature)
            feature in
      List.iter (fun row ->
        Buffer.add_string buf row;
        Buffer.add_char buf '\n') rows
    ) ann
  let to_string = to_string_via_buffer to_buffer
  let to_file = to_file_via_buffer to_buffer
end

