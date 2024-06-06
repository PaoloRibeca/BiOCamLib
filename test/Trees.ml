open BiOCamLib
open Better

type end_t =
  | Leaf of int
  | Node of int

let () =
  let leaf_info =
    [| "A", [ "color", "red"; ];
       "F", [];
       "C", [];
       "D", [];
       "H", [];
       "B", [];
       "E", [];
       "G", [];
       "Z", [] |]
  and node_info =
    [| [|Leaf 0; Leaf 1|], [|2.,-1.,-1.,false; 1.,-1.,-1.,false|], None, [];
       [|Leaf 2; Leaf 3|], [|1.,-1.,-1.,false; 3.,-1.,-1.,false|], None, [];
       [|Leaf 8|], [|1.,-1.,-1.,false|], Some (Trees.Newick.Recombination 1), [];
       [|Leaf 4; Node 1; Node 2|], [|2.,-1.,-1.,false; 5.,-1.,-1.,false; 1.,-1.,-1.,true|], None, [];
       [|Node 0; Node 3|], [|4.,-1.,-1.,false; 4.,-1.,-1.,false|], None, [];
       [|Leaf 5; Leaf 6; Node 2|], [|1.,-1.,-1.,false; 2.,-1.,-1.,false; 2.,-1.,-1.,false|], None, [];
       [|Node 4; Node 5|], [|2.,-1.,-1.,false; 8.,-1.,-1.,false|], None, [];
       [|Leaf 7; Node 6|], [|10.,-1.,-1.,false; 1.,-1.,-1.,false|], None, [] |] in
  let leaves =
    Array.map
      (fun (name, dict) -> Trees.Newick.leaf ~dict:(List.to_seq dict |> StringMap.of_seq) name)
      leaf_info in
  let nodes = Array.make (Array.length node_info) leaves.(0) in
  Array.iteri
    (fun node_i (subs, edges, hybrid, dict) ->
      let make_edge i =
        let (length, bootstrap, probability, is_ghost) = edges.(i) in
        Trees.Newick.edge ~length ~bootstrap ~probability ~is_ghost () in
      let subs =
        Array.mapi
          (fun i -> function
            | Leaf n -> make_edge i, leaves.(n)
            | Node n -> make_edge i, nodes.(n))
          subs in
      let node =
        Trees.Newick.set_hybrid
          (Trees.Newick.join ~dict:(List.to_seq dict |> StringMap.of_seq) subs)
          hybrid in
      nodes.(node_i) <- node)
    node_info;
  let t = nodes.(Array.length nodes - 1) in
  Trees.Newick.to_string t |> Printf.printf "%s\n%!";
  Trees.Newick.to_string ~rich_format:false t |> Printf.printf "%s\n%!";
  Trees.Newick.set_is_root (Trees.Newick.to_string t |> Trees.Newick.of_string) true |> Trees.Newick.to_string |> Printf.printf "%s\n%!";
  let t = Trees.Newick.of_file "test/phylo_tb_truth.nwk" in
  Trees.Newick.to_string ~rich_format:false t |> Printf.printf "%s\n%!";
  let ft = Trees.Newick.dfs_flatten t in
  Array.iteri
    (fun i (prev_edge, prev_idx, node, descs) ->
      Printf.printf "%d[\"%s\"] (%d[%.10g])<--(+)-->(" i (Trees.Newick.get_node_name node) prev_idx (Trees.Newick.get_edge_length prev_edge);
      Array.iter
        (fun (edge, node_idx) ->
          Printf.printf "%d[%.10g]" node_idx (Trees.Newick.get_edge_length edge))
        descs;
      Printf.printf ")\n%!")
    ft;
  let ds = Trees.Newick.dijkstra ft 0 in
  Float.Array.iteri
    (fun i d ->
      let _, _, n, _ = ft.(i) in
      Printf.printf "%d[\"%s\"] %.10g\n%!" i (Trees.Newick.get_node_name n) d)
    ds;
  let dm = Trees.Newick.get_distance_matrix t in
  Matrix.to_file dm "test/phylo_tb_truth.dm.txt"

