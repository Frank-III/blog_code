(** It works*)
type 'a btree = 
  | Leaf of 'a
  | Tree of 'a * 'a btree * 'a btree

let print_tree t = 
  let open Core in   
  let repeat_str n str = 
    let rec repeat_tail sum = function 
    | 0 -> sum 
    | x -> repeat_tail (sum ^ str) (x-1) 
  in repeat_tail "" n in 
  let rec print_w_indent indent = function 
  | Leaf a -> print_endline ((repeat_str indent " ") ^ (Int.to_string a))
  | Tree (v, left, right) -> 
    print_endline ((repeat_str indent " ") ^ (Int.to_string v));
    (print_w_indent (indent + 2) left);
    (print_w_indent (indent + 2) right);
  in 
  print_w_indent 0 t;;

let rec dfs_tree t = 
  let open Core in 
  match t with 
  | Leaf a -> printf "%d " a
  | Tree (a, l, r) -> 
    printf "%d " a;
    dfs_tree l;
    dfs_tree r;
  ;;

let bfs_tree t = 
  let open Core in 
  let rec search = function 
  | [] -> print_endline ""
  | hd :: tl -> 
    match hd with 
    | Leaf a -> 
      printf "%d " a;
      search tl;
    | Tree (a, l, r) -> 
      printf "%d " a;
      search (List.rev (r :: l :: tl))
  in search [t]

(* let () = 
  let t = Tree (6, Tree(5, Leaf 3, Leaf 2), Leaf 1) in 
  print_tree t;
  dfs_tree t;
  print_endline "";
  bfs_tree t;; *)



    
