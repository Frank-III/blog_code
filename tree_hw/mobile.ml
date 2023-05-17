type 'a arm = 'a * 'a mobile 

and 'a mobile = 
  | Planet of 'a 
  | Mobile of  'a arm * 'a arm
;;

let rec total_weight = function 
  | Planet x -> x
  | Mobile (left, right) -> (arm_weight left) + (arm_weight right) 

and arm_weight arm = 
  let _, connected = arm in 
  (total_weight connected)
;;


let balanced = function
  | Planet _ -> true
  | Mobile ((len_l, rod_l), (len_r, rod_r)) -> 
      len_l * (total_weight rod_l) = len_r * (total_weight rod_r)

let total_trees t =
  let open Btree in 
  let rec con_sum_tree = function 
  | Planet x -> Leaf x 
  | Mobile ((_, rod_l), (_, rod_r)) as x -> 
    Tree ((total_weight x), (con_sum_tree rod_l), (con_sum_tree rod_r))
  in con_sum_tree t;;
  ;;





let () = 
  let open Core in 
  let t = Mobile ((1, Planet 2), (2, Planet 1)) in 
  let u = Mobile ((5, Planet 1), (1, Mobile ((2, Planet 3), (3, Planet 2)))) in 
  let v = Mobile ((4, t), (2, u)) in 
  let w = Mobile ((3, t), (2, u)) in 
  printf "The total weight of t is: %d\n" (total_weight t);
  printf "The total weight of u is: %d\n" (total_weight u);
  printf "The total weight of v is: %d\n" (total_weight v);
  printf "t is balanced: %b\n" (balanced t);
  printf "w is balanced: %b\n" (balanced w);
  printf "v is balanced: %b\n" (balanced v);
  Btree.(print_tree (total_trees t));
  Btree.(print_tree (total_trees u));
  Btree.(print_tree (total_trees v));
;;