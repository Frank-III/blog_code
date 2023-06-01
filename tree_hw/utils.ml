let @@ f x = f x

let return x = Some x

let (>>=) x f = match x with
  | Some x -> f x
  | None -> None

let (>>>=) v (x,f) = match v,x with
  | None, Some x -> Some (f x)
  | _ -> v

(* let (>>|) *)

let (<$>) f x = match x with
  | Some x -> Some (f x)
  | None -> None

let ( let* ) o f =
  match o with
  | None -> None
  | Some x -> f x

let return x = Some x

module Result = struct
  type 'a result = Success of 'a | Failure of string

  let return x = Success x

  let default x = function
    | Success x -> x
    | Failure _ -> x

  let (>>=) x f = match x with
    | Success x -> f x
    | Failure err -> Failure err

  let (<$>) f = function
    | Success x -> Success (f x) (* could be write as return @@ f x*)
    | Failure err -> Failure err
end
