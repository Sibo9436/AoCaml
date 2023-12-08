open Aoc

type node = Node of string * string * string 
let _print_node =function |Node (a,b,c)  ->  Format.printf "%s: (%s, %s)\n" a b c

let parse_node line = 
  let (let*) = Option.bind in 
  let* name, rest = String.split_once line '=' in 
  let* left,right = String.split_once rest ',' in 
  let left =String.trim left and right = String.trim right in 
  let left = String.sub left   1 (String.length left - 1) in  
  let right  = String.sub right   0 (String.length right - 1) in  
  Some (Node ( String.trim name, left, right ))
;;

let rec gcd a b = match (a,b) with 
  | (x, 0) -> x
  | (x,y) -> gcd y (x mod y)
;;
let lcm a b = (a*b) / gcd a b
let listcm = List.fold_left ( fun acc x -> lcm acc x) 1 

let rec _checkall = function 
  | [] -> true 
  | x::xs -> if String.get x 2 <> 'Z' then  false else _checkall xs

let () = let lines = handle_args|> seq_of_file in 
  let instructions, rest = match lines () with  |Seq.Nil -> failwith"casino" | Seq.Cons(instructions, rest) -> (instructions, rest) in 
  let nodes = Seq.filter ((<>)String.empty)  rest |> Seq.map (Fun.compose Option.get parse_node) |> Seq.memoize in 
  print_endline instructions;
  let map = Seq.map (function |Node(a,b,c) -> (a,(b,c))) nodes |> Hashtbl.of_seq in Hashtbl.iter (fun k (a,b) -> Format.printf "Map: %s->%s,%s\n" k a b) map;
  let instructions = instructions |> String.to_seq |> Seq.cycle in 
  let move_pred predicate = 
  let rec _move inst nodename count=  match inst () with  
    | Seq.Cons('L',xs) -> let (left,_) = Hashtbl.find map nodename in if predicate left  then count else _move xs left (count+1)
    | Seq.Cons('R',xs) -> let (_,right) = Hashtbl.find map nodename in if predicate right  then count else _move xs right (count+1)
    | _ -> failwith "wait what"
    in _move 
  in  let result = move_pred (fun s -> s = "ZZZ") instructions "AAA" 1 
  in print_endline "Day08"; 
  Format.printf "Part 1: %d\n" result ; 
  let starting_nodes = Seq.filter (function | Node(s,_,_) -> String.get s 2 = 'A') nodes |> List.of_seq
  in 
    let steps_to_z = List.map (fun i -> move_pred (fun s -> String.get  s 2= 'Z') instructions i 1 ) (List.map (function | Node(s,_,_) -> s) starting_nodes )in 

    let result = listcm steps_to_z in Format.printf "Part 2: %d\n" result;
