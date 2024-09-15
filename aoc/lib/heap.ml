type ord= Less | Equal | Greater
module type Ordering  = sig
  type  t
  val compare:   t -> t -> ord
end

module Make (T: Ordering) : sig
  type t =  Node of T.t * t * t| Leaf
  val pure : T.t -> t
  val map : (T.t -> T.t) -> t -> t
  val height : t -> int
  val push : T.t -> t -> t
  val pop : t -> T.t option * t
  
end = 
struct
  type ti = T.t
  type t =  Node of ti * t * t| Leaf
  let pure v = Node(v, Leaf, Leaf)
  let rec lheight = function 
    |Leaf -> 0
    | Node(_,l,_) -> 1+ lheight l 
  let rec rheight = function
    | Leaf -> 0
    | Node(_, _, r) -> 1+ rheight r
  let rec height =
    let lg a b = if a<b then b else a in  function 
      |Leaf -> 0
      | Node (_,l,r) -> 1 + lg (height l)(height r) 


  let swap p c  rl=
    match p with
    | Node (v, l, r) -> begin
        match c with
        | Node (cv,cl, cr) when rl =0 -> begin match T.compare cv v with
            | Greater -> Node(cv,Node(v,cl,cr),r)
            | Less | Equal -> Node(v,c, r)
          end
        | Node(cv, cl, cr) when rl =1 -> begin match T.compare cv v with 
            | Greater -> Node(cv,l,Node(v,cl,cr))
            | Less | Equal -> Node(v,l, c)
          end 
        | _ -> Leaf  
      end
    | _ -> Leaf

  let push n = let is_full t = lheight t = rheight t in 
    let rec auxpush =
      function 
      | Leaf -> pure n
      | Node(_, l, r) as node -> 
        if is_full node then swap node (auxpush l) 0
        else if is_full l then swap node (auxpush r) 1 
        else swap node (auxpush l) 0
    in auxpush

    let rec pop_rightmost = function 
      | Leaf -> (None, Leaf) 
      | Node (v, Leaf, Leaf) -> (Some v, Leaf) 
      | Node (v, l, r ) when lheight l = lheight r -> let (op, nt)    = pop_rightmost r  in (op, Node(v,l, nt))
      | Node (v, l, r )  ->   let (op, nt)    = pop_rightmost l  in (op, Node(v,nt, r))

  let map f = function | Leaf -> Leaf | Node(v,l,r) -> Node(f v,l,r) 
  let  pop = 
    let rec balance = function 
      | Leaf -> Leaf
      | Node (_, Leaf,Leaf) as n -> n 
      | Node(v,(Node (lv,ll,lr)as l),(Node (rv,rl,rr) as r)) -> 
        if T.compare lv rv = Greater then begin
          if T.compare lv v = Greater then 
            Node(lv, balance @@ Node(v, ll, lr), r)
          else Node(v, l, r) end
        else if T.compare rv v = Greater then 
          Node(rv, l,  balance @@ Node(v, rl, rr))
        else Node(v, l, r)
        (* questo penso si possa ridurre introducendo una funzione che compari 
           direttamente i nodi e per cui Leaf < Node *)
      | Node(v, (Node (lv, ll, lr)as l), Leaf) -> 
        if T.compare lv v = Greater then 
          Node(lv, balance @@ Node(v, ll, lr), Leaf)
        else Node(v, l, Leaf)
      | Node(v, Leaf, (Node (rv, rl, rr) as r)) -> if T.compare rv v = Greater then 
          Node(rv, Leaf,  balance @@ Node(v, rl, rr))
        else Node(v, Leaf, r)
    in function 
      |Leaf -> (None, Leaf)
      | Node(c,_,_) as tree -> let op, tr= pop_rightmost tree  in
        match op with 
        | None -> (None, tr)
        | Some (v) -> (Some c, balance @@ map (Fun.const v) tr )
end



