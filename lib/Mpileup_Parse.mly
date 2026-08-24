(*
    Mpileup_Parse.mly -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Mpileup_Parse.mly is the grammar of the read-bases column of a
    pileup line.  It is flat -- the column is a sequence of
    what each read says, with nothing nested -- and what it is for is
    the shape of one read's contribution: a caret and a mapping
    quality before the base, an indel and a dollar after it, each
    optional and each meaning something different.  Writing that out
    as a rule says it once, where a hand-rolled scan says it as a
    scattering of flags.

    Qualities are not here.  They are a column of their own, one
    character per call, and joining the two is done by Mpileup once
    both have been read -- which is also the only place able to say
    that they disagreed about how many calls there were.

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

%{
open Mpileup_Base
%}

(* The strand is in the name rather than in a payload: a constant constructor
   is immediate, where one carrying even a boolean is a block allocated for
   every call in the column, and '.' and ',' are most of a pileup *)
%token Mp_REFERENCE_FWD
%token Mp_REFERENCE_REV
%token<char> Mp_BASE_FWD
%token<char> Mp_BASE_REV
%token Mp_GAP_FWD
%token Mp_GAP_REV
%token Mp_SKIP_FWD
%token Mp_SKIP_REV
%token<string> Mp_INSERTION
%token<string> Mp_DELETION
%token<int> Mp_START
%token Mp_END
%token Mp_EOF

(* The quality is left at zero here and filled in by [Mpileup] from the column
   that carries it *)
%start<Mpileup_Base.Read.t list> read_bases

%%

(* The list comes back reversed, and deliberately.  Menhir's [list] is
   right-recursive, which obliges an LR parser to shift every token of the
   column before it can reduce any of them: a stack as deep as the pileup, a
   cell of it allocated per call.  Left recursion reduces as it goes and keeps
   the stack constant.  The caller is filling an array from this anyway, and can
   fill it backwards for nothing. *)
read_bases:
  | calls = reads_reversed Mp_EOF { calls }

reads_reversed:
  | { [] }
  | rest = reads_reversed r = one_read { r :: rest }

one_read:
  | starts = option(Mp_START) c = call indel = option(indel) ends = boption(Mp_END)
    {
      let call, strand = c in
      { Read.call = call;
        strand = strand;
        quality = 0;
        indel = indel;
        starts_read = starts;
        ends_read = ends }
    }

(* Inlined, so that the pair below is substituted into the action above rather
   than built and taken apart again once per call *)
%inline call:
  | Mp_REFERENCE_FWD { Call.Reference, Sequences.Types.forward }
  | Mp_REFERENCE_REV { Call.Reference, Sequences.Types.reverse }
  | b = Mp_BASE_FWD { Call.Base b, Sequences.Types.forward }
  | b = Mp_BASE_REV { Call.Base b, Sequences.Types.reverse }
  | Mp_GAP_FWD { Call.Gap, Sequences.Types.forward }
  | Mp_GAP_REV { Call.Gap, Sequences.Types.reverse }
  | Mp_SKIP_FWD { Call.Skip, Sequences.Types.forward }
  | Mp_SKIP_REV { Call.Skip, Sequences.Types.reverse }

%inline indel:
  | s = Mp_INSERTION { Indel.Insertion s }
  | s = Mp_DELETION { Indel.Deletion s }
