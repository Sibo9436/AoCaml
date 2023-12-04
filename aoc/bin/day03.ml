open Aoc


let get_surr rows cols = 
  let surr x y = 
    [
      if x-1 >= 0   && y-1 >= 0   then Some(x-1,y-1) else None;
      if               y-1 >= 0   then Some(x,y-1) else None;
      if x+1 < cols && y-1 >= 0   then Some(x+1,y-1) else None;

      if x-1 >= 0                 then Some(x-1,y) else None;
      if x+1 < cols               then Some(x+1,y) else None;

      if x-1 >= 0   && y+1 < rows then Some(x-1,y+1) else None;
      if               y+1 < rows then Some(x,y+1) else None;
      if x+1 < cols && y+1 < rows then Some(x+1,y+1) else None;
    ]
  in surr 


let sumnumbers hotpos l = 
  let is_hot pos = let rec help = function 
      | [] -> false
      | y::_ when y = pos-> true
      | _::ys -> help ys 
    in help hotpos
  in let rec readline ln sq = match sq () with 
      | Seq.Nil -> 0
      | Seq.Cons((_idx,c),_) as cons when  Char.is_digit c -> 
        begin 
          let is_hotpos = ref false in
          let rec readnum numsq = match numsq() with 
            | Seq.Cons((nidx, nc),ns) when Char.is_digit nc ->let a,b = readnum ns in if is_hot (nidx,ln) then is_hotpos := true ; (a,(Char.escaped nc)^b)
            | v -> (v,"")
          in let rest, value = readnum (fun () -> cons) 
          in let value = if !is_hotpos then begin (* Format.printf "Found at %d-%d: %s\n" _idx ln value;*) int_of_string @@ value end else 0
          in value  + readline ln (fun ()-> rest)
        end
      | Seq.Cons((_,_),ys) -> readline ln ys
  in let rec read ln = function 
      | Seq.Nil -> 0
      | Seq.Cons(y,ys) -> (readline ln (String.to_seqi y)) + read (ln+1) (ys ())
  in read 0 l


let map_adj l= 
  (* I guess sometimes side effects are just a thing of life °-^ *)
  (* Feels like spaghetti, I have to get better at this *)
  let mat = Array.of_seq (
      Seq.map (fun x -> Array.of_seq @@ String.to_seq x) l
    )
  in 
  let surr = get_surr (Array.length mat.(0)) (Array.length mat) in
  let findthem = Array.to_list @@Array.mapi ( fun y arr -> 
      Array.to_list @@Array.mapi (fun x c ->
          if  not (Char.is_digit c) && c != '.' 
          then surr x y else []
        ) arr
    )  mat
  in let hotpos = List.flatten @@ List.flatten findthem
  in let rec clean = function 
      | [] -> []
      | Some v :: rest -> v::clean rest 
      | None::rest -> clean rest
  in clean hotpos

let map_gear l= 
  (* It's late at night and I'm tired, don't @ me *)
  let mat = Array.of_seq (
      Seq.map (fun x -> Array.of_seq @@ String.to_seq x) l
    )
  in 
  let surr = get_surr (Array.length mat.(0)) (Array.length mat) in
  let findthem = Array.to_list @@Array.mapi ( fun y arr -> 
      Array.to_list @@Array.mapi (fun x c ->
          if  c = '*'
          then surr x y else []
        ) arr
    )  mat
  in let hotpos = List.flatten   findthem
  in let rec clean = function 
      | [] -> []
      | Some v :: rest -> v::clean rest 
      | None::rest -> clean rest
  in List.map clean hotpos


type readdir = Right|Left |Both
let sumgears hotpos l = 
  let mat = Array.of_seq (
      Seq.map (fun x -> Array.of_seq @@ String.to_seq x) l
    )
  in 
  let cols=  (Array.length mat.(0)) and rows = (Array.length mat) in
  let rec readnum x y dir = let helper c = 
    match (c, dir) with 
    | (c, Both) when Char.is_digit c -> let sx,sy, rleft =  readnum  (x-1) y Left and _,_,rright = readnum  (x+1) y Right in(sx,sy, rleft^ (Char.escaped c)  ^ rright)
    | (c, Right) when Char.is_digit c -> let _,_,rright =readnum  (x+1) y Right in  (x,y, (Char.escaped c)  ^ rright)
    | (c, Left) when Char.is_digit c -> let sx,sy,rleft = readnum  (x-1) y Left in (sx,sy,rleft ^ (Char.escaped c)  )
      | _ -> (x+1,y,"")
  in if x >=0 && x < cols && y >= 0 && y < rows then 
    helper mat.(y).(x) else (x,y,"")
  in 
  List.fold_left (fun acc ht -> (Hashtbl.fold (fun (_,_) value akk -> (int_of_string value) * akk ) ht  1) + acc ) 0 
  @@
  List.filter (fun li -> Hashtbl.length li = 2)
  @@List.map (fun star -> 
    Hashtbl.of_seq @@ List.to_seq @@ List.map (fun (sx,sy,s) -> ((sx,sy),s))
    (List.filter (fun (_,_,s) -> s != "")
    (List.map (fun (x, y) -> readnum x y Both) star))
  ) hotpos





let () = let filename = handle_args in 
  let getline = seq_of_file filename
  in 
  let memo = Seq.memoize getline
  in
  print_endline "Part One";
  print_int @@begin Format.print_flush (); sumnumbers (map_adj memo) (memo ())end;
  print_endline "Part Two";
  print_int @@ sumgears (map_gear memo) (memo );
  print_char '\n'
(* Format.printf "%i\n"@@ solvefirst (getline ()) *)
