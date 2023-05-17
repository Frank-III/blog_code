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


(**more work: dfs or bfs*)
let has_path t word = 
  let rec rec_search acc = function 
  | Leaf x -> (acc ^ x) = word
  | Tree (v, trees) ->
    let concat_ = acc ^ v in 
    (concat_ = word) || (Core.List.fold trees ~f:(fun acc x -> (acc || (rec_search concat_ x))) ~init:false)
  in rec_search "" t 



let () = 
  let open Core in
  let t = Tree ("odin", [Tree ("balder", [Leaf "loki"; Leaf "freya"]); Tree ("frigg", [Leaf "loki"]); Tree ("loki", [Leaf "sif"; Leaf "loki"]); Leaf "loki"]) in 
  Tree.print_tree t;
  let modified_t = replace_loki_at_leaf t "lokis" in 
  Tree.print_tree modified_t;
  let greeting = Tree ("h", [Leaf "i"; 
                            Tree ("e", [Tree ("l", [Tree ("l", [Leaf "o"])]);
                                        Leaf "y"])]) in 
  print_tree greeting;
  printf "has h: %b\n" (has_path greeting "h");
  printf "has i: %b\n" (has_path greeting "i");
  printf "has hi: %b\n" (has_path greeting "hi");
  printf "has hey: %b\n" (has_path greeting "hey");
  printf "has hello: %b\n" (has_path greeting "hello");
  printf "has bye: %b\n" (has_path greeting "bye");
  printf "has hint: %b\n" (has_path greeting "hint");