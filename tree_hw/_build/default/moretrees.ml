open Tree

let rec replace_loki_at_leaf t lokis_replacement= 
  match t with 
  | (Leaf a) as l -> 
    (if a = "loki" then (Leaf lokis_replacement) else l)
  | Tree (v, trees) -> 
    Tree (
      (if v = "loki" then lokis_replacement else v),
      (Core.List.map ~f:(fun x -> replace_loki_at_leaf x lokis_replacement) trees)
    )
  ;;


let () = 
  let t = Tree ("odin", [Tree ("balder", [Leaf "loki"; Leaf "freya"]); Tree ("frigg", [Leaf "loki"]); Tree ("loki", [Leaf "sif"; Leaf "loki"]); Leaf "loki"]) in 
  Tree.print_tree t;
  print_endline "";
  let modified_t = replace_loki_at_leaf t "lokis" in 
  Tree.print_tree modified_t;
