(*
    RunTests.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    RunTests.ml is the entry point of the BiOCamLib test suite.  It
    runs every registered per-area suite in turn and prints the
    summary, exiting non-zero when a check failed or when a
    known-bug marker has gone stale.  Adding coverage for another
    part of the library means adding a Tests_<Area>.ml alongside
    this file, listing it in test/dune, and adding one line below.

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

let () =
  Printf.eprintf "%s\n%!" (String.TermIO.bold "BiOCamLib test suite");
  Tests_Annotations.run ();
  Testing.summary ()

