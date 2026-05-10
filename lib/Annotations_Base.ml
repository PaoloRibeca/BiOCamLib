(*
    Annotations_Base.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Annotations_Base.ml provides the format-independent building blocks
    for the genome-annotation toolkit: the per-component interning
    tables (paths, sequence names, attribute keys, attribute values),
    the [Hierarchy] schema, the [Annotation] AST, and the
    [GenBankLocation] AST that the Menhir parser populates.
    Format-specific parsers, writers, and the binary I/O live in
    [Annotations].

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

(* The implicit root every hierarchy is wrapped in.  Users describe
   their hierarchy as a forest of top-level categories; the parser
   stitches them under this label so the data structure stays a
   single tree.  Paths returned by [Path.to_list] always start with
   this name. *)
let implicit_root_name = "annotation"

(* GenBank LOCATION AST.  Coordinates here are 1-based inclusive
   (the source-format convention); the [Annotations]-level driver
   converts to 0-based half-open when populating
   [Annotation.feature_t.intervals]. *)
module GenBankLocation = struct
  type endpoint_t = {
    pos: int;
    fuzzy_left: bool; (* leading "<" *)
    fuzzy_right: bool (* leading ">" -- only on the upper bound *)
  }
  type t =
    | Point of endpoint_t
    | Range of endpoint_t * endpoint_t
    | Between of int * int (* "100^101" *)
    | Complement of t
    | Join of t list
    | Order of t list
    | Remote of string * int option * t (* accession[.version]:t *)
end

(* GenBank record AST: the parser's output, before the Annotations
   driver lifts it into a [feature_t] with interned components.
   [headers] keeps every column-0 keyword line in source order,
   with continuation lines already stitched into the value;
   [features] holds [(name, raw_location, qualifiers)] triples,
   each qualifier value already with its continuation lines
   joined; [origin] is the concatenated lower-case sequence body
   when ORIGIN was present, [None] otherwise. *)
module GenBankRecord = struct
  type feature_t = {
    name: string;
    location: string;
    qualifiers: (string * string) list
  }
  type t = {
    headers: (string * string) list;
    features: feature_t list;
    origin: string option
  }
end

(* Hash-consed path identifier.  Every AST node stores ONE [Path.t]
   -- the full chain of categories from the implicit root down to
   and including the node's own category.  The string-list view is
   recovered via [Path.to_list], passing the [Path.Table.t] that
   the owning [Annotation.t] holds. *)
module Path:
  sig
    type t = private int
    module Table:
      sig
        type t
        val create: unit -> t
        val intern: t -> string list -> int
        val to_list: t -> int -> string list
        val cardinal: t -> int
      end
    val intern: Table.t -> string list -> t
    val to_list: Table.t -> t -> string list
    val to_string: ?sep:string -> Table.t -> t -> string
    val of_string: ?sep:string -> Table.t -> string -> t
    val leaf_category: Table.t -> t -> string
    val equal: t -> t -> bool
    val compare: t -> t -> int
    val hash: t -> int
  end
= struct
    type t = int
    module Table = struct
      type t = {
        mutable next_id: int;
        to_id: (string list, int) Hashtbl.t;
        from_id: (int, string list) Hashtbl.t
      }
      let create () = {
        next_id = 0;
        to_id = Hashtbl.create 64;
        from_id = Hashtbl.create 64
      }
      let intern table path =
        match Hashtbl.find_opt table.to_id path with
        | Some id -> id
        | None ->
          let id = table.next_id in
          table.next_id <- id + 1;
          Hashtbl.add table.to_id path id;
          Hashtbl.add table.from_id id path;
          id
      let to_list table id =
        try Hashtbl.find table.from_id id
        with Not_found ->
          Exception.raise __FUNCTION__ Algorithm
            (Printf.sprintf "Path id %d not found in table" id)
      let cardinal table = Hashtbl.length table.from_id
    end
    let intern = Table.intern
    let to_list = Table.to_list
    let path_to_string ?(sep = "->") = String.concat sep
    let to_string ?sep table p =
      path_to_string ?sep (to_list table p)
    let of_string ?(sep = "->") table s =
      if s = "" then intern table []
      else
        let n = String.length s and m = String.length sep in
        if m = 0 then intern table [s]
        else
          let buf = Buffer.create 32 and acc = ref [] in
          let i = ref 0 in
          while !i < n do
            if !i + m <= n && String.sub s !i m = sep then begin
              List.accum acc (Buffer.contents buf);
              Buffer.clear buf;
              i := !i + m
            end else begin
              Buffer.add_char buf s.[!i];
              incr i
            end
          done;
          List.accum acc (Buffer.contents buf);
          intern table (List.rev !acc)
    let leaf_category table p =
      match List.rev (to_list table p) with
      | [] -> ""
      | x :: _ -> x
    let equal a b = a = b
    let compare = Stdlib.compare
    let hash = Hashtbl.hash
  end

(* Generic single-string interner used by [Seq] and [AttrKey].
   Distinct generative functor instantiations keep the [t]s
   abstract per consumer. *)
module MakeStringIntern () = struct
  type t = int
  module Table = struct
    type t = {
      mutable next_id: int;
      to_id: (string, int) Hashtbl.t;
      from_id: (int, string) Hashtbl.t
    }
    let create () = {
      next_id = 0;
      to_id = Hashtbl.create 32;
      from_id = Hashtbl.create 32
    }
    let intern table s =
      match Hashtbl.find_opt table.to_id s with
      | Some id -> id
      | None ->
        let id = table.next_id in
        table.next_id <- id + 1;
        Hashtbl.add table.to_id s id;
        Hashtbl.add table.from_id id s;
        id
    let to_string table id =
      try Hashtbl.find table.from_id id
      with Not_found ->
        Exception.raise __FUNCTION__ Algorithm
          (Printf.sprintf "Interner id %d not found" id)
    let cardinal table = Hashtbl.length table.from_id
  end
  let intern = Table.intern
  let to_string = Table.to_string
  let equal a b = a = b
  let compare = Stdlib.compare
  let hash = Hashtbl.hash
end

(* Interned identifier for a sequence/contig name.  Each distinct
   name (e.g. "chr1", "chrM", "ENA|XXX") gets one int; storing
   2.2 M GENCODE rows then carries 2.2 M ints referring to ~25
   unique entries instead of 2.2 M heap strings. *)
module Seq = MakeStringIntern ()

(* Interned identifier for an attribute key (column-9 keys like
   "ID", "Parent", "gene_id", ...).  GENCODE has ~20 distinct keys
   across millions of rows -- interning collapses the duplication. *)
module AttrKey = MakeStringIntern ()

(* Per-feature attribute map: keys are interned [AttrKey.t], values
   are a [Value.t array] so genuinely repeated qualifiers (GenBank
   /db_xref, GFF3 comma lists split on parse) can be preserved
   without lossy joining, in a flat layout that's ~half the size of
   a cons-cell list. *)
module AttrMap = Map.Make (Int)

(* Adaptive value representation.  The first time a value string is
   seen during parse it is stored as [Value.String s]; the
   second-and-subsequent occurrences trigger promotion in
   [ValueTable] and are stored as [Value.Hashed id].  An end-of-
   parse cleanup pass walks every feature's attribute arrays and
   rewrites already-promoted [Value.String]s in place, so the
   post-parse invariant is "[Value.String s] iff [s] occurs exactly
   once across the annotation".  Callers see the original string
   regardless, via [Annotation.attr_get] / [Annotation.attr_iter]. *)
module Value = struct
  type t =
    | Hashed of int
    | String of string
end

(* Compact "have I seen this string at least once?" sketch.
   Replaces the full forward [string -> state] hashtable that
   would otherwise grow to ~one entry per unique value seen during
   parse (hundreds of MB for GENCODE-sized inputs).  Default sized
   for ~1 M items at 1 % fpp: 1.2 MB, ~7 hash bits per query.
   False positives manifest as a brand-new singleton being promoted
   on first sight -- correct (still resolves via [to_string]) just
   slightly suboptimal. *)
module Bloom = struct
  type t = {
    bits: Bytes.t;
    m: int; (* bit count -- always a multiple of 8 *)
    k: int (* number of hash slots queried per op *)
  }
  (* Default sized for a 200 Mbp-class chromosome's worth of unique
     strings.  GENCODE chr1 is ~700 k unique attribute values; 1 M
     capacity at 1 % fpp gives us comfortable headroom. *)
  let create ?(capacity = 1_000_000) ?(fpp = 0.01) () =
    let n = float_of_int capacity
    and p = fpp in
    let ln2 = log 2. in
    let m_raw = -. n *. log p /. (ln2 *. ln2) in
    let m =
      let m = int_of_float (Float.ceil m_raw) in
      let m = max m 64 in
      (m + 7) / 8 * 8 in
    let k =
      max 1
        (int_of_float (Float.round (m_raw /. n *. ln2))) in
    { bits = Bytes.make (m / 8) '\000'; m; k }
  let bit_set t bit =
    let b = bit lsr 3 and mask = 1 lsl (bit land 7) in
    let cur = Char.code (Bytes.unsafe_get t.bits b) in
    Bytes.unsafe_set t.bits b (Char.chr (cur lor mask))
  let bit_test t bit =
    let b = bit lsr 3 and mask = 1 lsl (bit land 7) in
    let cur = Char.code (Bytes.unsafe_get t.bits b) in
    cur land mask <> 0
  let add t s =
    let h1 = Hashtbl.hash s
    and h2 = Hashtbl.seeded_hash 0xCAFEBABE s in
    for i = 0 to t.k - 1 do
      let h = (h1 + i * h2) land 0x3FFFFFFF in
      bit_set t (h mod t.m)
    done
  let mem t s =
    let h1 = Hashtbl.hash s
    and h2 = Hashtbl.seeded_hash 0xCAFEBABE s in
    let result = ref true and i = ref 0 in
    while !result && !i < t.k do
      let h = (h1 + !i * h2) land 0x3FFFFFFF in
      if not (bit_test t (h mod t.m)) then result := false;
      incr i
    done;
    !result
end

(* Adaptive interner for attribute values.  The Bloom filter is the
   singleton tracker; the forward hashtable only carries PROMOTED
   entries (the small population of values that occur >= 2 times).
   [drop_bloom] releases the Bloom (e.g. at sequence boundaries
   during parse, or end-of-parse altogether); the next [intern]
   call lazily allocates a fresh empty Bloom. *)
module ValueTable:
  sig
    type t
    val create: unit -> t
    val intern: t -> string -> Value.t
    val to_string: t -> Value.t -> string
    val find_id: t -> string -> int option
    val cardinal: t -> int
    val drop_bloom: t -> unit
  end
= struct
    type t = {
      mutable bloom: Bloom.t option;
      forward: (string, int) Hashtbl.t;
      inverse: (int, string) Hashtbl.t;
      mutable next_id: int
    }
    let create () = {
      bloom = Some (Bloom.create ());
      forward = Hashtbl.create 256;
      inverse = Hashtbl.create 256;
      next_id = 0
    }
    let ensure_bloom table =
      match table.bloom with
      | Some b -> b
      | None ->
        let b = Bloom.create () in
        table.bloom <- Some b;
        b
    let intern table s =
      match Hashtbl.find_opt table.forward s with
      | Some id -> Value.Hashed id
      | None ->
        let bloom = ensure_bloom table in
        if Bloom.mem bloom s then begin
          let id = table.next_id in
          table.next_id <- id + 1;
          Hashtbl.add table.forward s id;
          Hashtbl.add table.inverse id s;
          Value.Hashed id
        end else begin
          Bloom.add bloom s;
          Value.String s
        end
    let to_string table = function
      | Value.String s -> s
      | Value.Hashed id ->
        (try Hashtbl.find table.inverse id
         with Not_found ->
           Exception.raise __FUNCTION__ Algorithm
             (Printf.sprintf "Value id %d not found" id))
    let find_id table s = Hashtbl.find_opt table.forward s
    let cardinal table = Hashtbl.length table.inverse
    let drop_bloom table = table.bloom <- None
  end

(* A [Hierarchy.t] is a tree of category labels describing the
   allowed nesting of features in an annotation.  Each node carries
   a string name and a list of allowed children; the same name may
   appear at multiple positions in the tree (e.g. ["exon"] under
   both ["mRNA"] and ["lncRNA"]), so location is identified by the
   full path from the root, NOT by name alone.  Hierarchies are
   used both as a schema check during parsing and as the basis for
   lossless path-based (de)serialisation. *)
module Hierarchy:
  sig
    type t
    val node: string -> t list -> t
    val name: t -> string
    val children: t -> t list
    val find: t -> path:string list -> t option
    val children_of: t -> path:string list -> string list
    val validate: t -> path:string list -> bool
    val to_string: t -> string
  end
= struct
    type t = { name: string; children: t list }
    let node name children = { name; children }
    let name t = t.name
    let children t = t.children
    let find t ~path =
      match path with
      | [] -> None
      | first :: rest when first = t.name ->
        let rec walk n = function
          | [] -> Some n
          | x :: rest ->
            (match List.find_opt (fun c -> c.name = x) n.children with
             | None -> None
             | Some c -> walk c rest) in
        walk t rest
      | _ -> None
    let children_of t ~path =
      match find t ~path with
      | None -> []
      | Some n -> List.map (fun c -> c.name) n.children
    let validate t ~path =
      match find t ~path with Some _ -> true | None -> false
    let rec node_to_string n =
      match n.children with
      | [] -> n.name
      | _ ->
        Printf.sprintf "(%s (%s))"
          n.name
          (List.map node_to_string n.children |> String.concat ", ")
    (* The implicit root is stripped on output so that
       [of_string (to_string h)] round-trips: the parser will
       re-wrap on the way in. *)
    let to_string t =
      if t.name = implicit_root_name then
        List.map node_to_string t.children |> String.concat ", "
      else
        node_to_string t
  end

(* The annotation AST: a forest of top-level features under the
   implicit "annotation" root, plus the four per-component interning
   tables (paths, sequence names, attribute keys, attribute values),
   an optional [Sequences.Reference.t] providing the underlying
   sequences, and a free-form metadata map for file-level keys
   (LOCUS fields, [##gff-version], ...). *)
module Annotation:
  sig
    (* Per-feature payload.  Coordinates are 0-based half-open
       (matching [Sequences.Types.simple_interval_t]).  For most
       features [intervals] is a single-element list; multi-
       interval features (GenBank's [join(...)] or GFF3 records
       sharing an ID) carry several sub-intervals in genomic
       order.  [strand] is [None] for unstranded features.
       [phase] is the CDS reading-frame offset (0/1/2);
       elsewhere [None].  [source] is interned via [ValueTable]
       since GFF3/GTF column 2 is a small repeating vocabulary. *)
    type feature_t = {
      seq: Seq.t;
      source: Value.t option;
      intervals: Sequences.Types.simple_interval_t list;
      strand: Sequences.Types.strand_t option;
      phase: int option;
      id: string option;
      attributes: Value.t array AttrMap.t
    }
    val empty_feature: feature_t
    type t
    val create: Hierarchy.t -> t
    val hierarchy: t -> Hierarchy.t
    val paths: t -> Path.Table.t
    val seqs: t -> Seq.Table.t
    val attr_keys: t -> AttrKey.Table.t
    val values: t -> ValueTable.t
    val seq_name: t -> feature_t -> string
    val intern_seq: t -> string -> Seq.t
    val feature_source: t -> feature_t -> string option
    val intern_source: t -> string -> Value.t
    val attr_get: t -> feature_t -> string -> string list option
    val attr_iter: t -> (string -> string list -> unit) -> feature_t -> unit
    val attr_set: t -> feature_t -> key:string -> values:string list -> feature_t
    val cleanup_values: t -> unit
    val reference: t -> Sequences.Reference.t option
    val set_reference: t -> Sequences.Reference.t -> t
    val get_metadata: t -> string -> string list
    val add_metadata: t -> key:string -> value:string -> t
    val all_metadata: t -> string list StringMap.t
    (* Append a feature at the given path.  The path must validate
       against the hierarchy.  Internal segments (everything but the
       last) must already exist as the most-recent feature at the
       previous level; this implements the "DFS-ordered insertion"
       semantics natural to GenBank and post-sort GFF3.  Format-
       specific parsers that get records out of order should buffer
       and topologically sort before calling [add]. *)
    val add: t -> path:string list -> feature_t -> t
    val iter: (path:Path.t -> feature_t -> unit) -> t -> unit
    val fold: (path:Path.t -> feature_t -> 'a -> 'a) -> 'a -> t -> 'a
    val iter_paths: (path:string list -> feature_t -> unit) -> t -> unit
    val fold_paths: (path:string list -> feature_t -> 'a -> 'a) -> 'a -> t -> 'a
    val path_to_string: ?sep:string -> string list -> string
    val path_of_string: ?sep:string -> string -> string list
  end
= struct
    type feature_t = {
      seq: Seq.t;
      source: Value.t option;
      intervals: Sequences.Types.simple_interval_t list;
      strand: Sequences.Types.strand_t option;
      phase: int option;
      id: string option;
      attributes: Value.t array AttrMap.t
    }
    let empty_feature = {
      seq = (Obj.magic 0 : Seq.t);
      source = None;
      intervals = [];
      strand = None;
      phase = None;
      id = None;
      attributes = AttrMap.empty
    }
    type node_t = {
      path: Path.t;
      feature: feature_t;
      sub: node_t list
    }
    type t = {
      hierarchy: Hierarchy.t;
      paths: Path.Table.t;
      seqs: Seq.Table.t;
      attr_keys: AttrKey.Table.t;
      values: ValueTable.t;
      reference: Sequences.Reference.t option;
      metadata: string list StringMap.t;
      forest: node_t list
    }
    let create hierarchy = {
      hierarchy;
      paths = Path.Table.create ();
      seqs = Seq.Table.create ();
      attr_keys = AttrKey.Table.create ();
      values = ValueTable.create ();
      reference = None;
      metadata = StringMap.empty;
      forest = []
    }
    let hierarchy t = t.hierarchy
    let paths t = t.paths
    let seqs t = t.seqs
    let attr_keys t = t.attr_keys
    let values t = t.values
    let intern_seq t name = Seq.intern t.seqs name
    let seq_name t f = Seq.to_string t.seqs f.seq
    let intern_source t s = ValueTable.intern t.values s
    let feature_source t f =
      Option.map (ValueTable.to_string t.values) f.source
    let value_array_to_strings t arr =
      Array.to_list arr
      |> List.map (ValueTable.to_string t.values)
    let attr_get t f key =
      let id = AttrKey.intern t.attr_keys key in
      AttrMap.find_opt id f.attributes
      |> Option.map (value_array_to_strings t)
    let attr_iter t f feature =
      AttrMap.iter (fun id arr ->
        f (AttrKey.to_string t.attr_keys id)
          (value_array_to_strings t arr)
      ) feature.attributes
    let attr_set t feature ~key ~values:str_values =
      let id = AttrKey.intern t.attr_keys key in
      let arr =
        Array.of_list
          (List.map (ValueTable.intern t.values) str_values) in
      { feature with
        attributes = AttrMap.add id arr feature.attributes }
    (* Walk every feature's attribute arrays and rewrite any
       [Value.String s] for which [s] now has a promoted id, in
       place.  After this pass, [Value.String s] is the unique
       carrier of [s] across the whole annotation. *)
    let cleanup_values t =
      let rec walk_nodes nodes =
        List.iter (fun n ->
          AttrMap.iter (fun _kid arr ->
            Array.iteri (fun i v ->
              match v with
              | Value.String s ->
                (match ValueTable.find_id t.values s with
                 | Some id -> arr.(i) <- Value.Hashed id
                 | None -> ())
              | Value.Hashed _ -> ()
            ) arr
          ) n.feature.attributes;
          walk_nodes n.sub
        ) nodes in
      walk_nodes t.forest;
      ValueTable.drop_bloom t.values
    let reference t = t.reference
    let set_reference t r = { t with reference = Some r }
    let get_metadata t key =
      try StringMap.find key t.metadata with Not_found -> []
    let add_metadata t ~key ~value =
      let prev = get_metadata t key in
      { t with metadata = StringMap.add key (prev @ [value]) t.metadata }
    let all_metadata t = t.metadata
    let path_to_string ?(sep = "->") = String.concat sep
    let path_of_string ?(sep = "->") s =
      if s = "" then []
      else
        let n = String.length s and m = String.length sep in
        if m = 0 then [s]
        else
          let buf = Buffer.create 32 and acc = ref [] in
          let i = ref 0 in
          while !i < n do
            if !i + m <= n && String.sub s !i m = sep then begin
              List.accum acc (Buffer.contents buf);
              Buffer.clear buf;
              i := !i + m
            end else begin
              Buffer.add_char buf s.[!i];
              incr i
            end
          done;
          List.accum acc (Buffer.contents buf);
          List.rev !acc
    let raise_invalid_path hierarchy path =
      Exception.raise __FUNCTION__ Algorithm
        (Printf.sprintf
           "Path %S is not valid against hierarchy {%s}"
           (path_to_string path) (Hierarchy.to_string hierarchy))
    let add t ~path feature =
      if not (Hierarchy.validate t.hierarchy ~path) then
        raise_invalid_path t.hierarchy path;
      match path with
      | _root :: (_ :: _ as rest) ->
        let leaf_path_id = Path.intern t.paths path in
        let rec drill prefix nodes = function
          | [] -> assert false
          | [_] ->
            nodes @ [{ path = leaf_path_id; feature; sub = [] }]
          | cat :: rest ->
            let prefix = prefix @ [cat] in
            let prefix_id = Path.intern t.paths prefix in
            let rec patch_last = function
              | [] ->
                Exception.raise __FUNCTION__ Algorithm
                  (Printf.sprintf
                     "No parent of category %S to attach feature at %s"
                     cat (path_to_string path))
              | [n] when Path.equal n.path prefix_id ->
                [{ n with sub = drill prefix n.sub rest }]
              | n :: ns -> n :: patch_last ns in
            patch_last nodes in
        let root_prefix = [ Hierarchy.name t.hierarchy ] in
        { t with forest = drill root_prefix t.forest rest }
      | _ ->
        Exception.raise __FUNCTION__ Algorithm
          (Printf.sprintf
             "Cannot attach a feature directly at the hierarchy \
              root %S" (path_to_string path))
    let iter f t =
      let rec walk nodes =
        List.iter (fun n ->
          f ~path:n.path n.feature;
          walk n.sub
        ) nodes in
      walk t.forest
    let fold f init t =
      let rec walk acc nodes =
        List.fold_left (fun acc n ->
          let acc = f ~path:n.path n.feature acc in
          walk acc n.sub
        ) acc nodes in
      walk init t.forest
    let iter_paths f t =
      iter (fun ~path feature ->
        f ~path:(Path.to_list t.paths path) feature) t
    let fold_paths f init t =
      fold (fun ~path feature acc ->
        f ~path:(Path.to_list t.paths path) feature acc) init t
  end

