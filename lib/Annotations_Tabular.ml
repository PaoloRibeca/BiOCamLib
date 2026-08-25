(*
    Annotations_Tabular.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Annotations_Tabular.ml reads and writes the tabular format: three
    normalised tables plus a FASTA sidecar, joined on a content hash,
    which together are the register's text twin.  It is the only
    format here that round-trips an [Annotation.t] whole.

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

(* Tabular: the register's text twin.  Three relations, none of which contains
   a nested syntax:
     <prefix>.AnnotationFeatures.txt    id parent seq path source score strand phase intervals
     <prefix>.AnnotationAttributes.txt  id key value
     <prefix>.AnnotationMetadata.txt    key value
   Every file has fixed arity, so [cat]ting two of them together is meaningful
   and adding one feature with a novel attribute key does not reshape any other
   row.  Attributes are normalised into their own relation rather than packed
   into a column, which is what lets a multi-valued attribute be several rows
   and a valueless one be a row with an empty third field -- neither of which
   GFF3's column 9 can express.
   A feature's id is a content hash chained through its parent, so it is stable
   under insertion and independent of row order: either file can be sorted and
   still [join]ed on the id.
   [to_file] / [of_file] take a PREFIX and use the three files.  The buffer and
   string entry points render the same content as one document with [#!] section
   banners, which is what makes the format usable down a pipe, diffable in one
   piece, and testable without touching the filesystem; a prefix under [/dev/*]
   selects that form too, matching the convention the binary writers use. *)
module Tabular: Format_t = struct
  (* The file itself carries the hierarchy it was written under, so this is only
     the fallback for a document that does not declare one.  A [--dialect] or
     [--hierarchy] override is meaningless here and AnnoTools refuses it rather
     than installing a default over a file that brought its own. *)
  let default_hierarchy = default_gff3_hierarchy
  let dialects = [ "standard", default_hierarchy ]
  let format_version = "2"
  let hash_recipe = "fnv1a64/1"
  (* FNV-1a over 64 bits, written out rather than reached for.  [Hashtbl.hash]
     would have been wrong twice over: it yields about 30 usable bits, where
     2.2 million features need 64 to keep the collision probability near 1e-7,
     and it is not guaranteed stable across OCaml releases, which for a value
     that goes into a file and has to mean the same thing on the way back is
     fatal. *)
  let hash_of_string s =
    let h = ref 0xcbf29ce484222325L in
    String.iter
      (fun c -> h := Int64.mul (Int64.logxor !h (Int64.of_int (Char.code c))) 0x100000001b3L) s;
    Printf.sprintf "%016Lx" !h
  (* Field encoding.  [%] always goes, so that a literal one cannot be read back
     as an escape; control bytes go, since a tab or a newline would end the
     field or the row; [>] goes in a path segment, so that the [->] separator
     can never occur inside one.  A field that would come out as exactly [.] is
     encoded too, because [.] is the absent marker. *)
  let encode ?(reserved = "") s =
    if s = "." then "%2E" else Annotations_Lex.url_encode ~reserved s
  let decode = Annotations_Lex.url_decode
  let encode_opt ?reserved = function
    | None -> "."
    | Some s -> encode ?reserved s
  let decode_opt = function
    | "." -> None
    | s -> Some (decode s)
  (* Intervals, in the INSDC spelling: 1-based inclusive, comma-joined, in the
     order stored rather than sorted, with a zero-length site as [lo^hi]. *)
  (* A piece is its one-based range, and then two things the range alone cannot
     say.  Partiality wraps it in the markers GenBank uses for the same purpose,
     [<] for a feature that began before the record and [>] for one that runs
     past its end -- here on the whole piece rather than on the endpoint, since
     this is our own format and the ends of a piece are unambiguous.  A piece on
     the opposite strand from its feature -- which only a mixed-strand join
     produces -- is wrapped in [complement(...)], GenBank's own spelling, so
     that what it means is legible without a key.
     Both have to be here or the format stops round-tripping, which is the one
     thing it exists for. *)
  let interval_to_field (s: Segment.t) =
    let body = OneBased.(of_interval s.span |> to_string) in
    let body =
      (if s.partial_low then "<" else "") ^ body ^ (if s.partial_high then ">" else "") in
    match s.strand with
    | Some (Sequences.Types.Reverse _) -> "complement(" ^ body ^ ")"
    | Some (Sequences.Types.Forward _) | None -> body
  let intervals_to_field intervals =
    match intervals with
    | [] -> "."
    | _ -> List.map interval_to_field intervals |> String.concat ","
  (* The exact inverse of the above, so that what this format writes is what it
     reads.  [OneBased] is what refuses a between-bases site whose positions are
     not consecutive: accepting any [hi] meant [100^999] parsed happily and was
     then re-emitted as [100^101], so a hand-edited file was silently rewritten
     rather than diagnosed -- and hand editing is what this format is for. *)
  (* The exact inverse of [interval_to_field], peeled in the order it wrapped *)
  let interval_of_field piece =
    let piece, strand =
      if String.starts_with ~prefix:"complement(" piece
         && String.ends_with ~suffix:")" piece then
        String.sub piece 11 (String.length piece - 12), Some Sequences.Types.reverse
      else
        piece, None in
    let partial_low = String.starts_with ~prefix:"<" piece in
    let piece = if partial_low then String.sub piece 1 (String.length piece - 1) else piece in
    let partial_high = String.ends_with ~suffix:">" piece in
    let piece = if partial_high then String.sub piece 0 (String.length piece - 1) else piece in
    Segment.make ~partial_low ~partial_high ?strand OneBased.(of_string piece |> to_interval)
  let intervals_of_field = function
    | "." | "" -> []
    | s -> String.Split.on_char_as_list ',' s |> List.map interval_of_field
  let strand_to_field = function
    | Some (Sequences.Types.Forward _) -> "+"
    | Some (Sequences.Types.Reverse _) -> "-"
    | None -> "."
  let phase_to_field = function
    | None -> "."
    | Some n -> string_of_int n
  (* The path is the category chain WITHOUT the implicit root, which is stripped
     on output and restored on input -- the same treatment [Hierarchy.to_string]
     gives it, and for the same reason: it is redundant on every row. *)
  let path_to_field path =
    match path with
    | root :: rest when root = implicit_root_name ->
      List.map (encode ~reserved:">") rest |> String.concat "->"
    | _ -> List.map (encode ~reserved:">") path |> String.concat "->"
  let path_of_field s =
    implicit_root_name
    :: (if s = "" || s = "." then []
        else String.Split.as_list (Str.regexp_string "->") s |> List.map decode)
  (* The format's own metadata keys live in a [!] namespace so that they cannot
     collide with an annotation's; a real key that happens to start with [!] is
     encoded on the way out. *)
  let own_key k = "!" ^ k
  (* A leading [!] would collide with that namespace, and a leading [#] would
     let a metadata row impersonate a section banner in the one-document form --
     a key of [#!features] would end the metadata table and start a second
     features one.  Both are escaped; [url_decode] restores them. *)
  let encode_metadata_key k =
    if k = "" then ""
    else
      let rest = encode (String.sub k 1 (String.length k - 1)) in
      match k.[0] with
      | '!' -> "%21" ^ rest
      (* A leading [#] would make the row look like a table header, and a
         leading [>] like the start of the reference. *)
      | '#' -> "%23" ^ rest
      | '>' -> "%3E" ^ rest
      | _ -> encode k
  (* The per-sequence translation table, recorded only when it is not the
     standard one, so an ordinary annotation carries no such rows at all. *)
  let table_key name = own_key ("table:" ^ name)
  (* [id] is the content hash and the join key; [feature_id] is the feature's
     OWN identifier, which is not always derivable from an attribute -- the
     GenBank reader names a record's source feature after its LOCUS -- and so
     needs a column of its own or it is simply lost.  The name matches the
     column --validate-report already uses for the same thing. *)
  (* A table opens with ONE line naming its columns, EACH prefixed with [#], as
     every tabular output in this family of tools spells a header.  That single
     rule replaces a banner plus a bare header row, and it is enough on its own:
     the three headers differ, so in the one-document form the header IS the
     table's identity and nothing else has to be agreed on.  A [>] line opens
     the reference, which is plain FASTA. *)
  let features_header =
    "#id\t#parent\t#seq\t#path\t#feature_id\t#source\t#score\t#strand\t#phase\t#intervals"
  let attributes_header = "#id\t#key\t#value"
  let metadata_header = "#key\t#value"
  let features_suffix = ".AnnotationFeatures.txt"
  let attributes_suffix = ".AnnotationAttributes.txt"
  let metadata_suffix = ".AnnotationMetadata.txt"
  let reference_suffix = ".AnnotationReference.fasta"
  let single_document prefix = String.length prefix >= 5 && String.sub prefix 0 5 = "/dev/"
  (* A prefix under [/dev/*] selects the one-document form, as it does for the
     binary writers.  So does an ordinary path that turns out to BE a document:
     someone handed a file rather than a prefix, and refusing it because its
     name lacks a suffix we invented would be unhelpful. *)
  let looks_like_document path =
    match open_in path with
    | exception _ -> false
    | ic ->
      (* [read] tolerates a preamble before the first banner, so the sniff has
         to skip the same thing rather than decide on line one -- otherwise a
         document carrying a comment header is taken for a prefix and the reader
         then goes looking for files that do not exist.  The first [#!] line
         settles it either way; anything that is not blank, not a comment and
         not a banner means this is not a document.  The line cap stops the
         sniff reading an arbitrary large file that merely begins with
         comments.
         [Sys_error] is caught alongside [End_of_file] because [open_in] on a
         directory succeeds on Linux and it is the READ that fails: without
         this the exception escaped a function whose whole job is to answer
         yes or no, and the channel leaked with it. *)
      let rec scan n =
        if n = 0 then false
        else
          match input_line ic with
          | exception (End_of_file | Sys_error _) -> false
          | line ->
            let trimmed = String.trim line in
            if trimmed = "" then scan (n - 1)
            else
              trimmed = metadata_header || trimmed = features_header
              || trimmed = attributes_header in
      let verdict = scan 100 in
      close_in_noerr ic;
      verdict
  (* Rendering.  Every feature is visited once, in DFS pre-order, and its id is
     computed from its identity chained through its parent's id -- so an id
     depends on where a feature sits, not merely on what it says, which is what
     separates the identical exons that alternative transcripts share. *)
  type rendered_t = {
    r_features: Buffer.t;
    r_attributes: Buffer.t;
    r_metadata: Buffer.t;
    (* Empty when no reference is attached, which is the GFF3-shaped case. *)
    r_reference: Buffer.t
  }
  let render ann =
    let r = {
      r_features = Buffer.create 4096;
      r_attributes = Buffer.create 4096;
      r_metadata = Buffer.create 256;
      r_reference = Buffer.create 4096
    } in
    (* The reference goes out as FASTA rather than into a table: a sequence is
       not tabular data, and a 30 kbp cell would be the same category error as
       packing attributes into one column.  As FASTA it is also readable by
       every other tool it might be handed to, and comes back in through the
       reader this library already has. *)
    Printf.bprintf r.r_metadata "%s\n" metadata_header;
    (match reference ann with
     | None -> ()
     | Some reference ->
       Sequences.Reference.iter (fun ~name ~seq:_ ~table ~description:_ ->
         if table <> Sequences.Translation.Table_1 then
           Printf.bprintf r.r_metadata "%s\t%s\n" (table_key name)
             (Sequences.Translation.to_string table)) reference;
       (* Unwrapped: every other line of a tabular document is one whole
          record, and the sequence is no different. *)
       write_fasta ~width:0 r.r_reference reference);
    Printf.bprintf r.r_metadata "%s\t%s\n" (own_key "format-version") format_version;
    Printf.bprintf r.r_metadata "%s\t%s\n" (own_key "hash-recipe") hash_recipe;
    Printf.bprintf r.r_metadata "%s\t%s\n" (own_key "hierarchy")
      (encode (Hierarchy.to_string (hierarchy ann)));
    StringMap.iter (fun k vs ->
      List.iter (fun v ->
        Printf.bprintf r.r_metadata "%s\t%s\n" (encode_metadata_key k) (encode v)) vs)
      (all_metadata ann);
    Buffer.add_string r.r_features features_header;
    Buffer.add_char r.r_features '\n';
    Buffer.add_string r.r_attributes attributes_header;
    Buffer.add_char r.r_attributes '\n';
    (* [id_of_path] remembers the id of the most recent feature seen at each
       path prefix, which in DFS pre-order is exactly the parent of whatever
       comes next at the level below. *)
    let id_of_path = Hashtbl.create 64
    and seen = Hashtbl.create 64
    and duplicates = Hashtbl.create 64 in
    iter_paths (fun ~path feature ->
      let depth = List.length path in
      let parent =
        if depth <= 2 then ""
        else
          match Hashtbl.find_opt id_of_path (List.filteri (fun i _ -> i < depth - 1) path) with
          | Some id -> id
          | None ->
            Exception.raise __FUNCTION__ Algorithm
              (Printf.sprintf "No parent id for %s: the walk is not in DFS order"
                 (path_to_string path)) in
      let category = match List.rev path with leaf :: _ -> leaf | [] -> "" in
      let intervals = intervals_to_field feature.intervals in
      let strand = strand_to_field feature.strand
      and phase = phase_to_field feature.phase in
      (* The feature's own identifier belongs in its identity, not in its
         payload: two alternative transcripts of one gene can agree on every
         structural field -- same span, same strand, same parent -- and differ
         only by their ID, and chaining through the parent alone would then give
         them, and every exon beneath them, the same id.
         Features that carry no identifier at all (a GenBank feature with
         neither /locus_tag nor /gene) can still be structurally identical
         siblings, so a counter distinguishes those.  It is keyed by the whole
         identity rather than by the parent, so it only ever moves off zero for
         an actual duplicate: adding an unrelated sibling does not renumber
         anything. *)
      let base =
        String.concat "\000"
          [ parent; Option.value ~default:"" feature.id; seq_name ann feature; category;
            intervals; strand; phase ] in
      let ordinal =
        match Hashtbl.find_opt duplicates base with
        | None -> Hashtbl.replace duplicates base 1; 0
        | Some n -> Hashtbl.replace duplicates base (n + 1); n in
      let key = Printf.sprintf "%s\000#%d" base ordinal in
      let id = hash_of_string key in
      (* Two features sharing an id would silently merge their attributes on the
         way back in, so the writer checks rather than trusting the arithmetic:
         everything is in memory here and the check costs one hashtable. *)
      (match Hashtbl.find_opt seen id with
       | Some prev when prev <> key ->
         Exception.raise __FUNCTION__ Algorithm
           (Printf.sprintf "Hash collision on %s: two distinct features share id %s" hash_recipe id)
       | _ -> Hashtbl.replace seen id key);
      Hashtbl.replace id_of_path path id;
      Printf.bprintf r.r_features "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n"
        id (if parent = "" then "." else parent) (encode (seq_name ann feature))
        (path_to_field path) (encode_opt feature.id) (encode_opt (feature_source ann feature))
        (field_of_score_exact feature.score) strand phase intervals;
      (* In source order, which is what the attributes now hold.  This used to
         sort, because the order was an interning artefact and a format whose
         point is to be diffable cannot inherit an order nobody controls -- but
         sorting now DISCARDS an order somebody does control, and a round trip
         through this format would put a feature's qualifiers back in the wrong
         sequence.  It is still deterministic, which is what diffability
         actually needs. *)
      let pairs = ref [] in
      attr_iter ann (fun k vs -> List.accum pairs (k, vs)) feature;
      List.rev !pairs
        |> List.iter (fun (k, vs) ->
          match vs with
          (* No values at all is not the same as one empty value, but the row
             shape cannot say both -- [<id> <key> <empty>] already means the
             latter, which is how a valueless qualifier travels.  An attribute
             with an empty value ARRAY is a degenerate state no reader produces
             (only [attr_set ~values:[]] can), and it means the same as the
             attribute being absent, so that is how it is written. *)
          | [] -> ()
          | _ ->
            List.iter
              (fun v ->
                Printf.bprintf r.r_attributes "%s\t%s\t%s\n" id (encode k) (encode v)) vs)) ann;
    r
  let to_buffer buf ann =
    let r = render ann in
    (* Each table already opens with its own header line, so they simply
       follow one another; the reference, being FASTA, goes last. *)
    Buffer.add_buffer buf r.r_metadata;
    Buffer.add_buffer buf r.r_features;
    Buffer.add_buffer buf r.r_attributes;
    Buffer.add_buffer buf r.r_reference
  let to_string = to_string_via_buffer to_buffer
  let to_file ann prefix =
    if single_document prefix then to_file_via_buffer to_buffer ann prefix
    else begin
      let r = render ann in
      List.iter (fun (suffix, buf) ->
        (* No reference means no FASTA file, rather than an empty one. *)
        if suffix <> reference_suffix || Buffer.length buf > 0 then begin
          let oc = open_out (prefix ^ suffix) in
          Buffer.output_buffer oc buf;
          close_out oc
        end)
        [ features_suffix, r.r_features;
          attributes_suffix, r.r_attributes;
          metadata_suffix, r.r_metadata;
          reference_suffix, r.r_reference ]
    end
  (* Reading.  Rows are collected first and the forest rebuilt from the parent
     column afterwards, so neither file has to arrive in any particular order:
     both may be sorted, which is most of the point of a tabular format. *)
  (* Split into lines, dropping a trailing CR the way [iter_tsv_lines] already
     does for GFF3 and GTF.  Without it a CRLF file carries the CR into the last
     field of every row: in the attributes table that is the value itself, which
     is then interned and re-emitted downstream as [ID=gene1%0D] -- silent
     corruption rather than a diagnosis. *)
  let lines_of s =
    String.Split.on_char_as_list '\n' s
    |> List.map (fun l ->
      let n = String.length l in
      if n > 0 && l.[n - 1] = '\r' then String.sub l 0 (n - 1) else l)
  (* Split one table's text into rows.  A leading [#] line is the header this
     table was recognised by and is checked; its absence is an error rather than
     something to guess around.  Only a genuinely EMPTY line is filler --
     trimming first would discard a metadata row whose key and value are both
     empty, which a bare [##] pragma in a GFF3 file produces. *)
  let split_rows header what s =
    let lines = lines_of s |> List.filter (fun l -> l <> "") in
    match lines with
    | [] -> []
    | first :: rest when String.trim first = header ->
      List.map (fun l -> String.Split.on_char_as_array '\t' l) rest
    | first :: _ ->
      Exception.raise __FUNCTION__ IO_Format
        (Printf.sprintf "Malformed %s table: expected header %S, found %S" what header
           (String.trim first))
  let field row i what n =
    if Array.length row <> n then
      Exception.raise __FUNCTION__ IO_Format
        (Printf.sprintf "Malformed %s row: expected %d fields, found %d" what n
           (Array.length row));
    row.(i)
  let read_tables ann_in ~features ~attributes ~metadata ~reference_fasta =
    (* Metadata first: it carries the hierarchy the rest has to validate
       against. *)
    let file_hierarchy = ref None and plain_metadata = ref []
    and tables = ref StringMap.empty in
    let table_prefix = own_key "table:" in
    List.iter (fun row ->
      let k = field row 0 "metadata" 2 and v = field row 1 "metadata" 2 in
      let is_table =
        String.length k > String.length table_prefix
        && String.sub k 0 (String.length table_prefix) = table_prefix in
      match k with
      | "!hierarchy" -> file_hierarchy := Some (Hierarchy.of_string (decode v))
      | "!format-version" ->
        if decode v <> format_version then
          Exception.raise __FUNCTION__ IO_Format
            (Printf.sprintf "Unsupported tabular format version %S (this is %S)" (decode v)
               format_version)
      | "!hash-recipe" -> ()
      | _ when is_table ->
        let name =
          decode (String.sub k (String.length table_prefix)
                    (String.length k - String.length table_prefix)) in
        tables := StringMap.add name (Sequences.Translation.of_string (decode v)) !tables
      | _ -> List.accum plain_metadata (decode k, decode v))
      (split_rows metadata_header "metadata" metadata);
    let is_empty = fold (fun ~path:_ _ _ -> false) true ann_in in
    let ann =
      match !file_hierarchy with
      | Some h when is_empty ->
        (* The register is rebuilt around the hierarchy the file declares, so
           everything on the carrier that is NOT the hierarchy has to be carried
           across -- its reference, and its metadata, which is register state in
           exactly the same way and was previously dropped without a word.  The
           carrier's entries go in first, so that the file's append after them,
           which is the order every other add-mode read already gives. *)
        let fresh = create h in
        let fresh =
          match reference ann_in with
          | Some r -> set_reference fresh r
          | None -> fresh in
        ref
          (StringMap.fold
             (fun k vs acc ->
               List.fold_left (fun acc v -> add_metadata acc ~key:k ~value:v) acc vs)
             (all_metadata ann_in) fresh)
      | Some h when Hierarchy.to_string h <> Hierarchy.to_string (hierarchy ann_in) ->
        Exception.raise __FUNCTION__ IO_Format
          "Tabular input declares a hierarchy that differs from the register's; \
           load it into an empty register instead"
      | _ -> ref ann_in in
    (* Attributes, grouped by feature id.  Values keep the order they appear in,
       which is what makes a multi-valued attribute round-trip. *)
    let attrs_of = Hashtbl.create 64 in
    List.iter (fun row ->
      let id = field row 0 "attributes" 3
      and k = decode (field row 1 "attributes" 3)
      and v = decode (field row 2 "attributes" 3) in
      let prev = try Hashtbl.find attrs_of id with Not_found -> [] in
      Hashtbl.replace attrs_of id ((k, v) :: prev))
      (split_rows attributes_header "attributes" attributes);
    let rows = split_rows features_header "features" features in
    let by_id = Hashtbl.create 64 and children = Hashtbl.create 64 and roots = ref [] in
    List.iter (fun row ->
      let id = field row 0 "features" 10 and parent = field row 1 "features" 10 in
      if Hashtbl.mem by_id id then
        Exception.raise __FUNCTION__ IO_Format
          (Printf.sprintf "Duplicate feature id %S in tabular input" id);
      Hashtbl.replace by_id id row;
      if parent = "." || parent = "" then List.accum roots id
      else begin
        let prev = try Hashtbl.find children parent with Not_found -> [] in
        Hashtbl.replace children parent (id :: prev)
      end) rows;
    (* An attributes row naming a feature that is not in the features table
       would attach to nothing and be dropped without a word. *)
    Hashtbl.iter (fun id _ ->
      if not (Hashtbl.mem by_id id) then
        Exception.raise __FUNCTION__ IO_Format
          (Printf.sprintf
             "The attributes table names feature %S, which the features table does not contain" id))
      attrs_of;
    (* Reconstruct DFS pre-order from the parent links.  Annotation.add wants
       each internal path segment to be the most recent node at the previous
       level, which is exactly what a depth-first walk of this forest gives it,
       whatever order the rows arrived in. *)
    let ordered = ref [] and reached = Hashtbl.create 64 in
    let rec walk parent_path id =
      (* A feature reached twice means the parent links are not a forest.  The
         completeness check below would not catch it on its own, since the count
         could still come out right. *)
      if Hashtbl.mem reached id then
        Exception.raise __FUNCTION__ IO_Format
          (Printf.sprintf "Feature %S is reachable twice: the parent links are not a forest" id);
      Hashtbl.replace reached id ();
      let row = Hashtbl.find by_id id in
      let path = path_of_field (field row 3 "features" 10) in
      (* The parent column and the path column describe one forest twice, and
         [Annotation.add] places a feature by its path alone.  If they disagree
         the file has no single meaning -- which of the two descriptions wins
         would depend on row order, and row order carrying no meaning is the
         whole point of the format. *)
      (match List.rev path with
       | _ :: rev_prefix when List.rev rev_prefix = parent_path -> ()
       | _ ->
         Exception.raise __FUNCTION__ IO_Format
           (Printf.sprintf
              "Feature %S has path %S, which does not sit directly below %S, the path of the \
               feature its parent column names"
              id (path_to_field path) (path_to_field parent_path)));
      let attrs =
        try List.rev (Hashtbl.find attrs_of id) with Not_found -> [] in
      let attr_map =
        List.fold_left (fun m (k, v) ->
          let kid = AttrKey.intern (attr_keys !ann) k in
          let existing = match Attributes.find_opt kid m with Some a -> Array.to_list a | None -> [] in
          Attributes.add kid
            (Array.of_list (existing @ [ ValueTable.intern (values !ann) v ])) m)
          Attributes.empty attrs in
      let feature = {
        seq = Seq.intern (seqs !ann) (decode (field row 2 "features" 10));
        source =
          Option.map (ValueTable.intern (values !ann))
            (decode_opt (field row 5 "features" 10));
        intervals = intervals_of_field (field row 9 "features" 10);
        score = score_of_field (field row 6 "features" 10);
        strand = strand_of_field (field row 7 "features" 10);
        phase = phase_of_field (field row 8 "features" 10);
        id = decode_opt (field row 4 "features" 10);
        attributes = attr_map
      } in
      List.accum ordered (path, feature);
      match Hashtbl.find_opt children id with
      | None -> ()
      | Some kids -> List.iter (walk path) (List.rev kids) in
    List.iter (walk [ implicit_root_name ]) (List.rev !roots);
    (* Every row has to have been reached.  A row whose parent column names an
       id that is not in the table, and a cycle among the parent links, are both
       invisible to the walk -- it simply never arrives -- so without this check
       such a feature would be dropped in silence. *)
    Hashtbl.iter (fun id _ ->
      if not (Hashtbl.mem reached id) then
        Exception.raise __FUNCTION__ IO_Format
          (Printf.sprintf
             "Feature %S is unreachable: its parent is not in the features table, or the \
              parent links contain a cycle" id)) by_id;
    add_dfs_with_seq_bloom ann (List.rev !ordered);
    List.iter (fun (k, v) -> ann := add_metadata !ann ~key:k ~value:v) (List.rev !plain_metadata);
    (* The reference travels as FASTA beside the tables.  Load it with the
       identity linter: what was written is what the register held, and folding
       an IUPAC code to N on the way back in would corrupt what a round trip is
       supposed to preserve.  Sequences with no recorded table get the standard
       one, which is why only the others are written out. *)
    if reference_fasta <> "" then begin
      let base =
        match Annotation.reference !ann with
        | Some r -> r
        | None -> Sequences.Reference.empty in
      ann :=
        Annotation.set_reference !ann
          (* Upper-cased and otherwise left alone, as for the other two ways a
             reference can arrive; see the note in the GFF3 reader. *)
          (Sequences.Reference.add_from_fasta_string ~linter:String.uppercase_ascii
             ~tables:!tables base reference_fasta)
    end;
    cleanup_values !ann;
    !ann
  (* One document.  A [#] line opens a table and names it -- the three headers
     differ, so nothing beyond the header itself has to be agreed on -- and a
     [>] line opens the reference, which is plain FASTA and runs to the end
     unless another [#] header follows it.  A row before any header, or a header
     that names no known table, is an error rather than something to skip. *)
  let read ann_in s =
    let sections = Hashtbl.create 4 in
    let current = ref None and buf = Buffer.create 1024 in
    let flush () =
      Option.iter (fun name -> Hashtbl.replace sections name (Buffer.contents buf)) !current in
    let open_section name line =
      flush ();
      Buffer.clear buf;
      current := Some name;
      Buffer.add_string buf line;
      Buffer.add_char buf '\n' in
    List.iter (fun line ->
      if line = "" then ()
      else if line.[0] = '#' then begin
        let trimmed = String.trim line in
        if trimmed = features_header then open_section "features" line
        else if trimmed = attributes_header then open_section "attributes" line
        else if trimmed = metadata_header then open_section "metadata" line
        else
          Exception.raise __FUNCTION__ IO_Format
            (Printf.sprintf "Tabular input: %S names no known table" trimmed)
      end else if line.[0] = '>' && !current <> Some "reference" then begin
        flush ();
        Buffer.clear buf;
        current := Some "reference";
        Buffer.add_string buf line;
        Buffer.add_char buf '\n'
      end else begin
        if !current = None then
          Exception.raise __FUNCTION__ IO_Format
            (Printf.sprintf "Tabular input: row %S appears before any table header" line);
        Buffer.add_string buf line;
        Buffer.add_char buf '\n'
      end) (lines_of s);
    flush ();
    let section name =
      match Hashtbl.find_opt sections name with
      | Some s -> s
      | None ->
        Exception.raise __FUNCTION__ IO_Format
          (Printf.sprintf "Tabular input has no %S table" name) in
    read_tables ann_in ~features:(section "features") ~attributes:(section "attributes")
      ~metadata:(section "metadata")
      ~reference_fasta:(Option.value ~default:"" (Hashtbl.find_opt sections "reference"))
  let read_from_file ann prefix =
    if single_document prefix || looks_like_document prefix then read ann (read_file prefix)
    else
      read_tables ann
        ~features:(read_file (prefix ^ features_suffix))
        ~attributes:(read_file (prefix ^ attributes_suffix))
        ~metadata:(read_file (prefix ^ metadata_suffix))
        (* A register with no reference writes no FASTA, so its absence is
           ordinary rather than an error. *)
        ~reference_fasta:
          (if Sys.file_exists (prefix ^ reference_suffix) then read_file (prefix ^ reference_suffix)
           else "")
  let of_string ?(hierarchy = default_hierarchy) s = read (create hierarchy) s
  let of_file ?(hierarchy = default_hierarchy) prefix = read_from_file (create hierarchy) prefix
end

