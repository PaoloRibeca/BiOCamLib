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

let strand_of_flag = function
  | true -> Sequences.Types.forward
  | false -> Sequences.Types.reverse
%}

(* The boolean each of these carries is the strand: true forward, false reverse *)
%token<bool> Mp_REFERENCE
%token<char * bool> Mp_BASE
%token<bool> Mp_GAP
%token<bool> Mp_SKIP
%token<string> Mp_INSERTION
%token<string> Mp_DELETION
%token<int> Mp_START
%token Mp_END
%token Mp_EOF

(* The quality is left at zero here and filled in by [Mpileup] from the column
   that carries it *)
%start<Mpileup_Base.Read.t list> read_bases

%%

read_bases:
  | calls = list(one_read) Mp_EOF { calls }

one_read:
  | starts = option(Mp_START) c = call indel = option(indel) ends = boption(Mp_END)
    {
      let call, forward = c in
      { Read.call = call;
        strand = strand_of_flag forward;
        quality = 0;
        indel = indel;
        starts_read = starts;
        ends_read = ends }
    }

call:
  | s = Mp_REFERENCE { Call.Reference, s }
  | b = Mp_BASE { let base, s = b in Call.Base base, s }
  | s = Mp_GAP { Call.Gap, s }
  | s = Mp_SKIP { Call.Skip, s }

indel:
  | s = Mp_INSERTION { Indel.Insertion s }
  | s = Mp_DELETION { Indel.Deletion s }
