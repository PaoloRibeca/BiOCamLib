{

  (*
      Annotations_Lex.mll -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

      This file is part of BiOCamLib, the OCaml foundations upon which
      a number of the bioinformatics tools I developed are built.

      Annotations_Lex.mll provides four token sources, one per
      grammar exposed by [Annotations_Parse]:
        * [hierarchy]        -- S-expression tokens for hierarchies
        * [gff_attributes]   -- GFF3 column-9 attribute tokens
        * [gtf_attributes]   -- GTF  column-9 attribute tokens
        * [genbank_location] -- GenBank LOCATION expression tokens
      Each rule consumes a string (passed as a [Lexing.lexbuf]
      built from it) and yields tokens until the matching [_EOF].

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

  let bad_char what c =
    Exception.raise __FUNCTION__ IO_Format
      (Printf.sprintf "Invalid character %S in %s" (String.make 1 c) what)

  (* GenBank records have three lexically distinct regions
     ([Headers], [Features], [Origin]) that the grammar above
     reflects.  The same indented-line shape disambiguates
     differently depending on which region we are in, so the
     [genbank] lex rule consults a mutable mode flag held here
     and reset to [Headers] on every [//] terminator. *)
  type genbank_mode = Gb_Headers | Gb_Features | Gb_Origin
  let genbank_mode = ref Gb_Headers
  let reset_genbank_mode () = genbank_mode := Gb_Headers

  (* Helpers used by the [genbank] lex action below.  They live
     here so the action stays a short dispatch on indent and
     mode rather than embedding their full text inline. *)
  let leading_spaces s =
    let n = String.length s in
    let rec loop i =
      if i < n && s.[i] = ' ' then loop (i + 1) else i in
    loop 0
  let split_indent s =
    let i = leading_spaces s in
    i, String.sub s i (String.length s - i)
  let strip_quotes v =
    let n = String.length v in
    if n >= 2 && v.[0] = '"' && v.[n - 1] = '"'
    then String.sub v 1 (n - 2) else v
  let split_first_space s =
    match String.index_opt s ' ' with
    | None -> s, ""
    | Some i ->
      let n = String.length s in
      let rec ltrim j = if j < n && s.[j] = ' ' then ltrim (j + 1) else j in
      String.sub s 0 i, String.sub s (ltrim (i + 1)) (n - ltrim (i + 1))

  (* Drop position counters and intra-row spaces from one
     ORIGIN body line, lower-casing whatever sequence bytes
     remain.  Standard GenBank lays each row out as a
     right-aligned 1-based position followed by 6 ten-base
     chunks separated by spaces; we tolerate any arrangement
     and just keep the alphabetic / dash / asterisk bytes. *)
  let extract_origin_chars s =
    let buf = Buffer.create (String.length s) in
    String.iter (fun c ->
      if (c >= 'a' && c <= 'z')
         || (c >= 'A' && c <= 'Z')
         || c = '-' || c = '*' then
        Buffer.add_char buf (Char.lowercase_ascii c)
    ) s;
    Buffer.contents buf

  (* Classify a non-blank line and emit the matching
     [Annotations_Parse] token.  Centralising the dispatch here
     keeps the lex rule's action a single function call. *)
  let genbank_token line =
    let indent, rest = split_indent line in
    if indent = 0 then begin
      if rest = "//" then begin
        reset_genbank_mode ();
        Annotations_Parse.Gb_RECORD_END
      end else
        let key, value = split_first_space rest in
        match key with
        | "FEATURES" ->
          genbank_mode := Gb_Features;
          Annotations_Parse.Gb_FEATURES_HEADER
        | "ORIGIN" ->
          genbank_mode := Gb_Origin;
          Annotations_Parse.Gb_ORIGIN_HEADER
        | _ -> Annotations_Parse.Gb_HEADER_LINE (key, value)
    end else
      match !genbank_mode with
      | Gb_Headers -> Annotations_Parse.Gb_HEADER_CONT_LINE rest
      | Gb_Origin -> Annotations_Parse.Gb_ORIGIN_LINE (extract_origin_chars rest)
      | Gb_Features ->
        if indent <= 5 then
          let name, loc = split_first_space rest in
          Annotations_Parse.Gb_FEATURE_LINE (name, loc)
        else if String.length rest > 0 && rest.[0] = '/' then
          let body = String.sub rest 1 (String.length rest - 1) in
          let key, value =
            match String.index_opt body '=' with
            | None -> body, ""
            | Some i ->
              String.sub body 0 i,
              strip_quotes
                (String.sub body (i + 1) (String.length body - i - 1)) in
          Annotations_Parse.Gb_QUAL_LINE (key, value)
        else
          Annotations_Parse.Gb_CONT_LINE rest

  (* GFF3 percent-decoding: %HH -> char.  Used at the
     parser-driver level via Annotations; also needed inside the
     lexer to keep VALUE tokens already decoded. *)
  let url_decode s =
    let n = String.length s in
    let buf = Buffer.create n in
    let i = ref 0 in
    while !i < n do
      let c = s.[!i] in
      if c = '%' && !i + 2 < n then begin
        let hex = String.sub s (!i + 1) 2 in
        try
          Buffer.add_char buf (Char.chr (int_of_string ("0x" ^ hex)));
          i := !i + 3
        with _ ->
          Buffer.add_char buf c;
          incr i
      end else begin
        Buffer.add_char buf c;
        incr i
      end
    done;
    Buffer.contents buf

}

(* Hierarchy S-expression.
   Whitespace and commas are token separators.  Names are
   non-empty maximal runs of any character outside the special
   set { ( ) , whitespace }. *)
rule hierarchy = parse
| [' ' '\t' '\r' '\n']+
  { hierarchy lexbuf }
| '('
  { Annotations_Parse.Hier_LPAREN }
| ')'
  { Annotations_Parse.Hier_RPAREN }
| ','
  { Annotations_Parse.Hier_COMMA }
| [^ ' ' '\t' '\r' '\n' '(' ')' ',']+ as s
  { Annotations_Parse.Hier_NAME s }
| eof
  { Annotations_Parse.Hier_EOF }
| _ as c
  { bad_char "hierarchy" c }

(* GFF3 attribute string.
   Format:  KEY=v1,v2;KEY=v3;...
   KEY is a non-empty run of bytes other than [= ; ,].  VALUE is
   a non-empty run of bytes other than [; ,], with %HH escapes
   decoded eagerly.  An attribute string of "." or "" yields the
   empty list. *)
and gff_attributes = parse
| [' ' '\t']+
  { gff_attributes lexbuf }
| '='
  { Annotations_Parse.Attr_EQ }
| ';'
  { Annotations_Parse.Attr_SEMI }
| ','
  { Annotations_Parse.Attr_COMMA }
| '.'
  { (* The GFF3 spec uses "." for an empty attribute string; we
       just consume it and let the surrounding grammar produce
       an empty list. *)
    gff_attributes lexbuf }
| [^ '=' ';' ',' ' ' '\t' '\r' '\n']+ as s
  { (* Single token kind disambiguated by 1-byte lookahead:
       runs followed by [=] are keys, otherwise values. *)
    let pos = lexbuf.Lexing.lex_curr_pos in
    let buf = lexbuf.Lexing.lex_buffer in
    let len = Bytes.length buf in
    let next =
      let i = ref pos in
      while !i < len &&
            (Bytes.get buf !i = ' ' || Bytes.get buf !i = '\t') do
        incr i
      done;
      if !i < len then Some (Bytes.get buf !i) else None in
    match next with
    | Some '=' -> Annotations_Parse.Attr_KEY s
    | _ -> Annotations_Parse.Attr_VALUE (url_decode s) }
| eof
  { Annotations_Parse.Attr_EOF }
| _ as c
  { bad_char "GFF3 attributes" c }

(* GTF attribute string.
   Format is a list of [KEY VALUE;] pairs where VALUE sits
   between double quotes.  Bytes inside the quotes are literal;
   backslash-quote and double-backslash escape the quote and
   backslash respectively (an Ensembl quirk seen in the wild). *)
and gtf_attributes = parse
| [' ' '\t']+
  { gtf_attributes lexbuf }
| ';'
  { Annotations_Parse.Attr_SEMI }
| '"'
  { Annotations_Parse.Attr_VALUE (gtf_quoted (Buffer.create 32) lexbuf) }
| [^ '"' ';' ' ' '\t' '\r' '\n']+ as s
  { Annotations_Parse.Attr_KEY s }
| eof
  { Annotations_Parse.Attr_EOF }
| _ as c
  { bad_char "GTF attributes" c }
and gtf_quoted buf = parse
| '\\' '"'
  { Buffer.add_char buf '"'; gtf_quoted buf lexbuf }
| '\\' '\\'
  { Buffer.add_char buf '\\'; gtf_quoted buf lexbuf }
| '"'
  { Buffer.contents buf }
| eof
  { Exception.raise __FUNCTION__ IO_Format
      "Unterminated quoted value in GTF attributes" }
| _ as c
  { Buffer.add_char buf c; gtf_quoted buf lexbuf }

(* GenBank LOCATION expression.
   Tokenises the contents of a feature's location field, which
   may span multiple lines after continuation-folding by the
   driver.  Whitespace is ignored. *)
and genbank_location = parse
| [' ' '\t' '\r' '\n']+
  { genbank_location lexbuf }
| ".."
  { Annotations_Parse.Loc_DOTDOT }
| '.'
  { Annotations_Parse.Loc_DOT }
| '^'
  { Annotations_Parse.Loc_CARET }
| '<'
  { Annotations_Parse.Loc_LT }
| '>'
  { Annotations_Parse.Loc_GT }
| '('
  { Annotations_Parse.Loc_LPAREN }
| ')'
  { Annotations_Parse.Loc_RPAREN }
| ','
  { Annotations_Parse.Loc_COMMA }
| ':'
  { Annotations_Parse.Loc_COLON }
| ['0'-'9']+ as n
  { try
      Annotations_Parse.Loc_INT (int_of_string n)
    with _ ->
      Exception.raise __FUNCTION__ IO_Format
        (Printf.sprintf "Invalid integer %S in GenBank location" n) }
| "complement"
  { Annotations_Parse.Loc_COMPLEMENT }
| "join"
  { Annotations_Parse.Loc_JOIN }
| "order"
  { Annotations_Parse.Loc_ORDER }
| ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']* as s
  { Annotations_Parse.Loc_NAME s }
| eof
  { Annotations_Parse.Loc_EOF }
| _ as c
  { bad_char "GenBank location" c }

(* GenBank record framing.
   The whole file is consumed line-by-line; each non-blank line
   becomes one of nine token classes via [genbank_token], which
   reads the modal state ref to disambiguate.  Blank lines and
   bare carriage returns are discarded.  The [//] terminator is
   the line content "//" at column 0; the [genbank_token] helper
   resets the mode to [Headers] when it emits [Gb_RECORD_END]. *)
and genbank = parse
| ['\n' '\r']+
  { genbank lexbuf }
| ([^ '\n' '\r']* as line) ('\r'? '\n')
  { let n = String.length line in
    let blank = ref true in
    for i = 0 to n - 1 do
      if line.[i] <> ' ' && line.[i] <> '\t' then blank := false
    done;
    if !blank then genbank lexbuf
    else genbank_token line }
| ([^ '\n' '\r']+ as line) eof
  { genbank_token line }
| eof
  { Annotations_Parse.Gb_EOF }

