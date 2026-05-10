%{

  (*
      Annotations_Parse.mly -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

      This file is part of BiOCamLib, the OCaml foundations upon which
      a number of the bioinformatics tools I developed are built.

      Annotations_Parse.mly groups every grammar that benefits from a
      proper Menhir parser, namely:

        * Hier_*    -- a small S-expression-like syntax for
                       annotation hierarchies, e.g.
                       (gene (mRNA (CDS exon intron)) miRNA)
        * Attr_*    -- the GFF3 / GTF attribute strings (column 9):
                          GFF3:  key=v1,v2;key2=v3
                          GTF:   key "v1"; key2 "v2";
        * Loc_*     -- GenBank location expressions, including
                       complement(...), join(...), order(...)
                       and partial markers (< / >).

      Per-line framing for GenBank records goes through the modal
      [genbank] rule in [Annotations_Lex.mll] and the
      [genbank_records] productions below; GFF3 and GTF are
      single-row tab-separated, so their per-row framing stays in
      plain OCaml inside [Annotations].

      Token names are prefixed (Hier_, Attr_, Loc_, Gb_) to keep the
      sections clear inside this single file.

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

%}

/* Hierarchy S-expression tokens */

%token Hier_LPAREN Hier_RPAREN Hier_COMMA Hier_EOF
%token<string> Hier_NAME

/* GFF3 / GTF attribute-string tokens.
   Both formats use the same token kinds and the same grammar
   below; what differs is the LEXER rule that produces them
   ([gff_attributes] versus [gtf_attributes] in
   Annotations_Lex.mll). */

%token<string> Attr_KEY Attr_VALUE
%token Attr_EQ Attr_SEMI Attr_COMMA Attr_EOF

/* GenBank LOCATION tokens.
   Cover [123], [<1..>500], [complement(123..456)],
   [join(1..100,200..300)], [order(...)], cross-record
   accession-prefixed locations [accession.1:1..100], etc. */

%token<int> Loc_INT
%token Loc_DOTDOT Loc_DOT Loc_CARET Loc_LT Loc_GT
%token Loc_LPAREN Loc_RPAREN Loc_COMMA Loc_COLON Loc_EOF
%token Loc_COMPLEMENT Loc_JOIN Loc_ORDER
%token<string> Loc_NAME

/* GenBank record tokens.
   Line-class tokens emitted by the modal [genbank] lexer rule.
   The lexer carries a [Headers | Features | Origin] mode so that
   the same indented-line shape disambiguates correctly across
   regions of a record.  Continuation lines are accumulated by
   parser actions (rather than fused at the lex level) so the
   grammar stays declarative. */

%token<string * string> Gb_HEADER_LINE
%token Gb_FEATURES_HEADER
%token Gb_ORIGIN_HEADER
%token<string * string> Gb_FEATURE_LINE
%token<string * string> Gb_QUAL_LINE
%token<string> Gb_CONT_LINE
%token<string> Gb_HEADER_CONT_LINE
%token<string> Gb_ORIGIN_LINE
%token Gb_RECORD_END
%token Gb_EOF

/* Entry points */

%start hierarchy
%type<Annotations_Base.Hierarchy.t> hierarchy

%start gff_attribute_list
%type<(string * string list) list> gff_attribute_list

%start gtf_attribute_list
%type<(string * string list) list> gtf_attribute_list

%start genbank_location
%type<Annotations_Base.GenBankLocation.t> genbank_location

%start genbank_records
%type<Annotations_Base.GenBankRecord.t list> genbank_records

%%

/* Hierarchy.
   The user-supplied input is the comma-separated list of
   children of an IMPLICIT root labelled [annotation]; the
   parser wraps them into a single-tree [annotation] node.
   Each child is either a bare [NAME] (leaf), the parenthesised
   form [(NAME)] (also a leaf), or
   [(NAME (child1, child2, ...))] -- a node with a non-empty,
   comma-separated child list wrapped in its own parens.
   Commas are mandatory between siblings; whitespace is
   otherwise free. */

hierarchy:
  | hierarchy_top_items Hier_EOF
    { Annotations_Base.Hierarchy.node
        Annotations_Base.implicit_root_name $1 }

hierarchy_top_items:
  | hierarchy_node
    { [ $1 ] }
  | hierarchy_node Hier_COMMA hierarchy_top_items
    { $1 :: $3 }

hierarchy_node:
  | Hier_NAME
    { Annotations_Base.Hierarchy.node $1 [] }
  | Hier_LPAREN Hier_NAME Hier_RPAREN
    { Annotations_Base.Hierarchy.node $2 [] }
  | Hier_LPAREN Hier_NAME Hier_LPAREN hierarchy_children Hier_RPAREN Hier_RPAREN
    { Annotations_Base.Hierarchy.node $2 $4 }

hierarchy_children:
  | hierarchy_node
    { [ $1 ] }
  | hierarchy_node Hier_COMMA hierarchy_children
    { $1 :: $3 }

/* GFF3 attribute string.
   Grammar:  list of [KEY=VALUE,VALUE,...] separated by [;].
   Empty list (a literal "." or "") is also valid.  Trailing
   semicolon is tolerated. */

gff_attribute_list:
  | Attr_EOF
    { [] }
  | gff_attribute_pair Attr_EOF
    { [ $1 ] }
  | gff_attribute_pair Attr_SEMI gff_attribute_list
    { $1 :: $3 }
  | Attr_SEMI gff_attribute_list
    { $2 }

gff_attribute_pair:
  | Attr_KEY Attr_EQ gff_attribute_values
    { $1, $3 }

gff_attribute_values:
  | Attr_VALUE
    { [ $1 ] }
  | Attr_VALUE Attr_COMMA gff_attribute_values
    { $1 :: $3 }

/* GTF attribute string.
   Grammar:  list of [KEY VALUE;] pairs.  KEY is a bareword; VALUE
   is a quoted string (the lexer strips the quotes).  Pairs are
   separated by semicolons; trailing semicolon required by the
   spec but tolerated here even when absent. */

gtf_attribute_list:
  | Attr_EOF
    { [] }
  | gtf_attribute_pair Attr_EOF
    { [ $1 ] }
  | gtf_attribute_pair Attr_SEMI gtf_attribute_list
    { $1 :: $3 }

gtf_attribute_pair:
  | Attr_KEY Attr_VALUE
    { $1, [ $2 ] }

/* GenBank LOCATION expression.
   Grammar covers everything in the INSDC feature-table
   specification that makes it through [.gb] files in practice,
   plus the partial-bound markers (<, >) and remote-accession
   references.  Anything more exotic raises through Menhir's
   default error path. */

genbank_location:
  | location Loc_EOF
    { $1 }

location:
  | endpoint
    { Annotations_Base.GenBankLocation.Point $1 }
  | endpoint Loc_DOTDOT endpoint
    { Annotations_Base.GenBankLocation.Range ($1, $3) }
  | Loc_INT Loc_CARET Loc_INT
    { Annotations_Base.GenBankLocation.Between ($1, $3) }
  | Loc_COMPLEMENT Loc_LPAREN location Loc_RPAREN
    { Annotations_Base.GenBankLocation.Complement $3 }
  | Loc_JOIN Loc_LPAREN location_list Loc_RPAREN
    { Annotations_Base.GenBankLocation.Join $3 }
  | Loc_ORDER Loc_LPAREN location_list Loc_RPAREN
    { Annotations_Base.GenBankLocation.Order $3 }
  | Loc_NAME maybe_version Loc_COLON location
    { Annotations_Base.GenBankLocation.Remote ($1, $2, $4) }

maybe_version:
  | /* EMPTY */
    { None }
  | Loc_DOT Loc_INT
    { Some $2 }

location_list:
  | location
    { [ $1 ] }
  | location Loc_COMMA location_list
    { $1 :: $3 }

endpoint:
  | Loc_INT
    { Annotations_Base.GenBankLocation.{
        pos = $1; fuzzy_left = false; fuzzy_right = false } }
  | Loc_LT Loc_INT
    { Annotations_Base.GenBankLocation.{
        pos = $2; fuzzy_left = true;  fuzzy_right = false } }
  | Loc_GT Loc_INT
    { Annotations_Base.GenBankLocation.{
        pos = $2; fuzzy_left = false; fuzzy_right = true  } }

/* GenBank records.
   The grammar reflects the three regions the lexer's modal
   tokeniser identifies:
     * a sequence of HEADER_LINEs (each with its own continuations)
       describing LOCUS / DEFINITION / ACCESSION / etc.;
     * an optional FEATURES section with FEATURE_LINEs, each
       carrying its location continuations and qualifier list;
     * an optional ORIGIN block whose body is a list of
       ORIGIN_LINE chunks.
   The whole thing is terminated by RECORD_END ([//]). */

genbank_records:
  | Gb_EOF
    { [] }
  | genbank_record genbank_records
    { $1 :: $2 }

genbank_record:
  | gb_headers gb_features gb_origin Gb_RECORD_END
    { Annotations_Base.GenBankRecord.{
        headers = $1;
        features = $2;
        origin = $3 } }

gb_headers:
  | /* EMPTY */
    { [] }
  | Gb_HEADER_LINE gb_header_continuations gb_headers
    { let key, value = $1 in
      let value =
        if $2 = [] then value
        else String.concat " " (value :: $2) in
      (key, value) :: $3 }

gb_header_continuations:
  | /* EMPTY */
    { [] }
  | Gb_HEADER_CONT_LINE gb_header_continuations
    { $1 :: $2 }

gb_features:
  | /* EMPTY */
    { [] }
  | Gb_FEATURES_HEADER gb_feature_list
    { $2 }

gb_feature_list:
  | /* EMPTY */
    { [] }
  | gb_feature gb_feature_list
    { $1 :: $2 }

gb_feature:
  | Gb_FEATURE_LINE gb_loc_continuations gb_qualifiers
    { let name, loc_stub = $1 in
      let location = String.concat "" (loc_stub :: $2) in
      Annotations_Base.GenBankRecord.{
        name; location; qualifiers = $3 } }

gb_loc_continuations:
  | /* EMPTY */
    { [] }
  | Gb_CONT_LINE gb_loc_continuations
    { $1 :: $2 }

gb_qualifiers:
  | /* EMPTY */
    { [] }
  | Gb_QUAL_LINE gb_qual_continuations gb_qualifiers
    { let key, value_stub = $1 in
      let value = String.concat "" (value_stub :: $2) in
      (key, value) :: $3 }

gb_qual_continuations:
  | /* EMPTY */
    { [] }
  | Gb_CONT_LINE gb_qual_continuations
    { $1 :: $2 }

gb_origin:
  | /* EMPTY */
    { None }
  | Gb_ORIGIN_HEADER gb_origin_lines
    { Some (String.concat "" $2) }

gb_origin_lines:
  | /* EMPTY */
    { [] }
  | Gb_ORIGIN_LINE gb_origin_lines
    { $1 :: $2 }

%%

