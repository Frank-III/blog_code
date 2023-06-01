open Core

type my_list = int list list [@@deriving show]

let combsum2 arr target: my_list =
  Array.sort ~compare:Int.compare arr;
  let n = Array.length arr in
  let res:  my_list ref = ref [] in
  let rec backtracking idx tar lst =
    if tar = target then (res := (lst :: !res))
    else if (idx = n) || (tar > target) then ()
    else (
      for j = idx to (n) - 1 do
        if (tar + arr.(j) > target) then ();
        backtracking (j+1) (tar + arr.(j)) (lst @ [arr.(j)])
      done)
  in
  backtracking 0 0 [];
  !res
;;


let () =
  let x = [|2;5;2;1;2|] in
  (* let y = Array.to_list x in *)
  let res = combsum2 x 8 in
  printf "%s" (show_my_list res)
  (* match res with *)
  (* | [] -> () *)
  (* | [_; b] -> List.iter ~f:(fun el -> printf "%d " el) b *)
  (* | _ -> print_endline "too much" *)
