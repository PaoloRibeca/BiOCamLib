%{
  (*open Better*)
%}

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

%%

/* The grammar */

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

