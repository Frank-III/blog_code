open Base;;
open Btree;;

let largest_exp = function 
  | [] -> 1
  | hd :: tl -> 
    let rec largest_val acc = function 
    | hd :: tl ->  Int.max (largest_val (acc ** hd) tl) (acc ** (largest_val hd tl))
    | [] -> acc
    in
    largest_val hd tl


(* let largest_exp_tree = function 
    | hd :: tl ->
      let rec con_largest_val_tree acc tree = function 
      | [] -> if Option.is_none tree then (acc, Leaf acc) else (acc, Tree (acc, Some tree))
      | hd :: tl -> 
        let first_t, first_acc = con_largest_val_tree (acc ** hd) (Tree ((acc ** hd), acc , hd)) tl in 
        let second_t, second_acc = con_largest_val_tree (hd) (Tree )  
      in 
      con_largest_val_tree hd None tl *)

(** Array is better here *)
let get_label = function 
  | Leaf a -> a
  | Tree(b, _, _) -> b

(** when Array is used the bug is just hard to see*)
let rec largest_exp_tree a = 
  if Array.length a = 1 then (Leaf (Array.get a 0))
  else 
    let tree_at_split i = 
      let base = largest_exp_tree (Array.sub a ~pos:0 ~len:i) in 
      let expon = largest_exp_tree (Array.sub a ~pos:i ~len:((Array.length a) - i)) in 
      Tree (((get_label base) ** (get_label expon)), base, expon)
    in 
  let subts = Array.map ~f:(tree_at_split) (Array.init ~f:(fun x -> x+1) (Array.length a - 1)) in
    (Option.value_exn (Array.max_elt subts ~compare:(fun a b -> (Int.compare (get_label a) (get_label b)))))
  ;;

let () =
  let open Core in 
  let t = [3;1;2;3] in 
  let t_a = Array.of_list t in 
  let u = [2;3;2] in 
  let u_a = Array.of_list u in 
  printf "The result of largest exp of t is: %d\n" (largest_exp t);
  print_tree (largest_exp_tree t_a);
  printf "The result of largest exp of u is: %d\n" (largest_exp u);
  print_tree (largest_exp_tree u_a);