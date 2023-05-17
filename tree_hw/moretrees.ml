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


(* let rec has_path t word = 
  match t with 
  | (Leaf a) -> 
    if a = word then true else false
  | Tree (v, trees) -> 
    (v = word) || (Core.List.fold ~f:(fun acc x -> acc || (has_path x word)) trees ~init:false)
  ;; *)


(**more work*)
let has_path t word = 
  let rec get_rest = function 
  | (Leaf a) -> a
  | Tree (v, trees) -> 
    Core.List.map ~f:(fun tree -> Core.List.fold tree ~f:(fun acc tree -> acc ^ (get_rest tree)) ~init:v) trees
  in



let () = 
  let t = Tree ("odin", [Tree ("balder", [Leaf "loki"; Leaf "freya"]); Tree ("frigg", [Leaf "loki"]); Tree ("loki", [Leaf "sif"; Leaf "loki"]); Leaf "loki"]) in 
  Tree.print_tree t;
  print_endline "";
  let modified_t = replace_loki_at_leaf t "lokis" in 
  Tree.print_tree modified_t;
