module type S = sig
  type key
  type 'a t
  val empty: 'a t
  val mem: key -> 'a t -> bool
  val add: key -> 'a -> 'a t -> 'a t
  val find: key -> 'a t -> 'a

end

module type OrderedType = sig
  type t
  val compare: t -> t -> int
end


module Make (Ord : OrderedType): S = struct
  type key = Ord.t

  type 'a t =
    | Empty
    | Node of {l : 'a t; v : key; d : 'a; r : 'a t; h : int}
      (** left subtree, key, value/data, right subtree, height of node *)

  let height m = m.h

  let empty = Empty

  let add nv nd m = 
    let rec add2map nv nd h = function 
    | Empty -> Node {l=Empty; v=nv; d=nd; r=Empty; h=h+1}
    | Node {l; v; d; r; h} as m-> 
      let c = Ord.compare nv v in 
      if c = 0 then Node {m with d = nd}
      else begin
        if c > 0 then add2map nv nd h l
        else add2map nv nd h r
      end
    in 
      add2map nv nd 0


  let rec mem x = function
    | Empty -> false
    | Node {l; v; r;_} ->
        let c = Ord.compare x v in
        c = 0 || mem x (if c < 0 then l else r)
end