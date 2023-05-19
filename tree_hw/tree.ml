type 'a tree = 
  | Leaf of 'a
  | Tree of 'a * 'a tree list
  

let print_tree t = 
  let open Core in   
    let repeat_str n str = 
      String.concat ~sep:"" (List.init n ~f:(fun _ -> str))
    in
    let rec print_w_indent indent = function 
    | Leaf a -> print_endline ((repeat_str indent " ") ^ (a))
    | Tree (v, trees) -> 
      print_endline ((repeat_str indent " ") ^ (v));
      List.iter ~f:(fun x -> print_w_indent (indent+2) x) trees
    in 
    print_w_indent 0 t;
    print_endline "\n";;


let rec dfs_tree t = 
  let open Core in 
  match t with 
  | Leaf a -> printf "%d " a
  | Tree (a, trees) -> 
    printf "%d " a;
    List.iter ~f:(dfs_tree) trees
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
    | Tree (a,  trees) -> 
      printf "%d " a;
      search (tl @ trees)
  in search [t]