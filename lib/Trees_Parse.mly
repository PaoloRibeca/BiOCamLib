%{

  (*
      Trees_Parse.mly -- (c) 2024-2025 Paolo Ribeca, <paolo.ribeca@gmail.com>

      This file is part of BiOCamLib, the OCaml foundations upon which
      a number of the bioinformatics tools I developed are built.

      Trees_Parse.mly implements parsers for:
       * phylogenetic trees represented in several dialects
          of the Newick format
       * list of weighted splits.

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

  (*open Better*)

%}

/* The tokens for Newick format */

%token Newick_LBRACK Newick_RBRACK Newick_COMMA Newick_COLON Newick_SEMI Newick_EOF
%token<bool> Newick_ROOTED
%token<string> Newick_NAME
%token<Trees_Base.Newick.hybrid_t> Newick_HYBRID
%token<float> Newick_LENGTH
%token<string Better.StringMap.t> Newick_DICT
/* No operators in this format */
/* Entry points */
%start zero_or_more_newick_trees
%type<Trees_Base.Newick.t list> zero_or_more_newick_trees
%start newick_tree
%type<Trees_Base.Newick.t> newick_tree

/* The tokens for weighted splits */

%token Splits_COLON Splits_SEMICOLON Splits_LBRACK Splits_RBRACK Splits_COMMA Splits_EOF
%token<string> Splits_NAME
%token<int> Splits_DECIMAL
%token<float> Splits_FLOAT
%token<Trees_Base.Splits.Split.t> Splits_BINARY
%token<Trees_Base.Splits.Split.t> Splits_OCTAL
%token<Trees_Base.Splits.Split.t> Splits_HEXADECIMAL
/* No operators in this format */
/* Entry points */
%start zero_or_more_split_sets
%type<Trees_Base.Splits.t list> zero_or_more_split_sets
%start split_set
%type<Trees_Base.Splits.t> split_set

%%

/* The grammar for Newick format */

zero_or_more_newick_trees:
  | Newick_EOF
    { [] }
  | newick_tree zero_or_more_newick_trees
    { $1 :: $2 }

newick_tree:
  | newick_opt_rooted newick_root_leaf Newick_SEMI
    { Trees_Base.Newick.set_is_root $2 $1 }
  | newick_opt_rooted Newick_LBRACK newick_branch Newick_COMMA one_or_more_newick_branches Newick_RBRACK newick_opt_name newick_opt_dict Newick_SEMI
    { let name, hybrid = $7 in
      let res = Trees_Base.Newick.join ~name ~dict:$8 (Array.of_list ($3 :: $5)) in
      Trees_Base.Newick.set_is_root (Trees_Base.Newick.set_hybrid res hybrid) $1 }

newick_opt_rooted:
  | /* EMPTY */
    { true }
  | Newick_ROOTED
    { $1 }

newick_root_leaf:
  | newick_opt_name newick_opt_dict /* Leaf */
    { let name, hybrid = $1 in
      Trees_Base.Newick.set_hybrid (Trees_Base.Newick.leaf ~dict:$2 name) hybrid }
  | Newick_LBRACK newick_branch Newick_RBRACK newick_opt_name newick_opt_dict
    { let name, hybrid = $4 in
      Trees_Base.Newick.set_hybrid (Trees_Base.Newick.leaf ~stem:(Some $2) ~dict:$5 name) hybrid }

one_or_more_newick_branches:
  | newick_branch
    { [ $1 ] }
  | newick_branch Newick_COMMA one_or_more_newick_branches
    { $1 :: $3 }

newick_branch:
  | newick_subtree newick_opt_lengths_and_dict
    { let length, bootstrap, probability, dict = $2 in
      Trees_Base.Newick.edge ~length ~bootstrap ~probability ~dict (), $1 }

newick_subtree:
  | newick_opt_name newick_opt_dict /* Leaf */
    { let name, hybrid = $1 in
      Trees_Base.Newick.set_hybrid (Trees_Base.Newick.leaf ~dict:$2 name) hybrid }
  | Newick_LBRACK one_or_more_newick_branches Newick_RBRACK newick_opt_name newick_opt_dict
    { let name, hybrid = $4 in
      Trees_Base.Newick.set_hybrid (Trees_Base.Newick.join ~name ~dict:$5 (Array.of_list $2)) hybrid }

newick_opt_name:
  | /* EMPTY */
    { "", None }
  | Newick_NAME
    { $1, None }
  | Newick_HYBRID
    { "", Some $1 }
  | Newick_NAME Newick_HYBRID
    { $1, Some $2 }

newick_opt_dict:
  | /* EMPTY */
    { Better.StringMap.empty }
  | Newick_DICT
    { $1 }

newick_opt_lengths_and_dict:
  | /* EMPTY */
    { -1., -1., -1., Better.StringMap.empty }
  | Newick_LENGTH newick_opt_dict
    { $1, -1., -1., $2 }
  | newick_opt_length Newick_LENGTH newick_opt_dict
    { $1, $2, -1., $3 }
  | newick_opt_length newick_opt_length Newick_LENGTH newick_opt_dict
    { $1, $2, $3, $4 }

newick_opt_length:
  | Newick_COLON
    { -1. }
  | Newick_LENGTH
    { $1 }

/* The grammar for weighted splits */

zero_or_more_split_sets:
  | Splits_EOF
    { [] }
  | split_set zero_or_more_split_sets
    { $1 :: $2 }

split_set:
  | zero_or_more_names Splits_COLON zero_or_more_comma_separated_weighted_splits Splits_SEMICOLON
    { let res = Array.of_list $1 |> Trees_Base.Splits.create in
      List.iter (fun (split, weight) -> Trees_Base.Splits.add_split res split weight) $3;
      res }

zero_or_more_names:
  | /* EMPTY */
    { [] }
  | Splits_NAME zero_or_more_names
    { $1 :: $2 }

zero_or_more_comma_separated_weighted_splits:
  | /* EMPTY */
    { [] }
  | weighted_split zero_or_more_comma_and_weighted_splits
    { $1 :: $2 }

zero_or_more_comma_and_weighted_splits:
  | /* EMPTY */
    { [] }
  | Splits_COMMA weighted_split zero_or_more_comma_and_weighted_splits
    { $2 :: $3 }

weighted_split:
  | weight Splits_LBRACK split Splits_RBRACK
    { $3, $1 }

weight:
  | Splits_DECIMAL
    { float_of_int $1 }
  | Splits_FLOAT
    { $1 }

split: /* Includes the empty case */
  | /* EMPTY */
    { Trees_Base.Splits.Split.of_list [] }
  | Splits_DECIMAL zero_or_more_comma_and_elements
    { Trees_Base.Splits.Split.of_list ($1 :: $2) }
  | Splits_BINARY
    { $1 }
  | Splits_OCTAL
    { $1 }
  | Splits_HEXADECIMAL
    { $1 }

zero_or_more_comma_and_elements:
  | /* EMPTY */
    { [] }
  | Splits_COMMA Splits_DECIMAL zero_or_more_comma_and_elements
    { $2 :: $3 }

