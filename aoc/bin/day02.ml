let usage_msg = "day02 <input_file>"

let input_file = ref ""
let anon filename = input_file := filename
let speclist = []
open Aoc

type color = Blue of int | Red of int | Green of int 



(* Like 3 blue, 1 red, 1 green *)
(* man parser combinators here would really ball *)
let parseset set = let colors = String.split_on_char ',' set in
(*the kind of unmaintainable hacks I love aoc for *)
  let splw s =  Option.get @@ String.split_once s ' ' in
  List.map (fun c ->let (num, colo) = print_endline c; splw (String.trim c) in let  v = int_of_string  num in 

  if colo = "green" then Green v else if colo = "blue" then Blue v 
  else if colo = "red" then Red v 
  else failwith @@ "this shouldn't have been reachable: "^colo) colors
  


let parseline line = let game, sets = Option.get @@ String.split_once (String.trim line) ':' (* for now we raise no biggie *)
  in let gameId =print_endline game; String.sub game 5 (String.length game - 5)
and  setlist = print_endline sets;  List.map (fun s -> s ) @@ String.split_on_char ';' (String.trim sets)
  in print_string_list setlist ; (int_of_string gameId, List.map (parseset) setlist)

let checkline l = let (i, cols) = l in  
let checknum = function 
  | Red   v -> v <= 12
  | Green v -> v <= 13
  | Blue  v -> v <= 14
  in 
let is_possible = List.fold_left (fun acc color -> acc && checknum color ) true  in
let are_possible = List.fold_left (fun acc p -> acc && is_possible p ) true in 
if are_possible cols then i else 0 

let find_smallest l = let (_, cols) = l in 
let flatten = List.flatten cols in
let lg = List.fold_left (fun acc x -> let (r,g,b)= acc in  match x with 
| Red   v when v > r -> (v,g,b)
| Green v when v > g -> (r,v,b)
| Blue  v when v > b -> (r,g,v)
| _ -> acc
) (0,0,0)
in let (r,g,b) = lg flatten in r*g*b

let () = Arg.parse speclist anon usage_msg;
  let getline =  Aoc.seq_of_file !input_file in
  let rec _solve_day_1 = function 
    | Seq.Nil -> 0
    | Seq.Cons(x,xs) -> print_endline x ; (checkline @@ parseline x) + (_solve_day_1 @@ xs ())
  in
  let rec solve = function 
    | Seq.Nil -> 0 
    | Seq.Cons(x,xs) -> ( find_smallest @@ parseline x) + (solve @@ xs ())
  in
  print_endline !input_file ; 
  print_int (solve @@ getline ());
  print_endline "\nDay one done";
  
