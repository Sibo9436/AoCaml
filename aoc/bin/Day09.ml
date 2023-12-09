open Aoc
let rec is_all_zero = function | [] -> true | x::xs -> if x <>0 then false else is_all_zero xs
;;
let rec newlist = function 
  | []| _::[] -> []
  | x::(y::_ as rest) -> (y-x)::newlist rest
;;
let rec find_next_val list = 
  if is_all_zero list then 0 else 
    let lastval = Option.get @@ List.last_opt list in 
    let res = lastval + find_next_val (newlist list)
    in print_int res ;print_char '\n'; res
;;
let rec find_prev_val list = 
  if is_all_zero list then 0 else 
    let firstval = List.hd list in 
    let res = firstval - find_prev_val (newlist list) 
    in print_int res ;print_char '\n'; res


let parse_history str =String.trim str |>  String.split_on_char ' ' |> List.map int_of_string
let () = let lines = handle_args |> seq_of_file |> List.of_seq |> List.map parse_history
  in 
  print_endline "Day09:"; 
  List.iter (fun li -> List.iter (Format.printf "%d ") li ; Format.print_newline ()) lines;
  let sum = List.map find_next_val lines |> List.fold_left (+) 0 
  in Format.printf "Part 1: %d\n" sum;
  let sum = List.map find_prev_val lines |> List.fold_left (+) 0 
  in Format.printf "Part 2: %d\n" sum



