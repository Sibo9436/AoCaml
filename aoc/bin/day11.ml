open Aoc
let rec of_opt_list = function 
  | None :: rest -> of_opt_list rest 
  | Some v :: rest -> v :: of_opt_list rest 
  | [] -> []

let find_empty list =List.init (List.length list)(Fun.id) |> List.map  (fun v -> if List.mem v list then None  else Some v) |> of_opt_list
let double_dist empties v = 
  List.fold_left (fun acc va -> if v > va then acc + 1 else acc ) v empties

let million_dist empties v = 
  List.fold_left (fun acc va -> if v > va then acc + 999999 else acc ) v empties


let shortest_paths galaxy other = let (x,y)=galaxy in List.map (fun (ox,oy) ->abs (x -ox) + abs (y - oy) ) other
let rec calc_min_dists  = function 
  | gal::rest -> shortest_paths gal rest :: calc_min_dists rest
  | [] -> []

let () = let map = handle_args |> seq_of_file |> Seq.map(fun s ->  String.fold_right (fun x acc -> x::acc) s [] ) |> List.of_seq
in let gal_pos = List.mapi(fun y v -> List.mapi (fun x c -> if c = '#' then Some (x,y) else None) v) map |>List.map of_opt_list |>List.flatten
in 
  List.iter (fun x -> List.iter (print_char) x; print_char '\n') map;
  List.iter (fun (x,y)-> print_int x ; print_char ' '; print_int y; print_char '\n') gal_pos; print_char '\n';
  let (full_c, full_r) = List.split gal_pos in 
  let e_r = (find_empty full_r) and e_c = find_empty full_c in
  let new_gal_pos =  List.combine (List.map (double_dist e_c) full_c) (List.map (double_dist e_r) full_r)
  in
  List.iter (fun x -> Format.printf "Row %d is empty\n" x) (e_r);
  List.iter (fun x -> Format.printf "Col %d is empty\n" x) (e_c);
  List.iter (fun (x,y) -> Format.printf "Galaxy at %d %d\n"x y)new_gal_pos;
  let res = calc_min_dists new_gal_pos |> List.flatten|> List.fold_left (+) 0  in Format.printf "Part One: %d\n" res;
  let new_gal_pos =  List.combine (List.map (million_dist e_c) full_c) (List.map (million_dist e_r) full_r)
  in 
  let res = calc_min_dists new_gal_pos |> List.flatten|> List.fold_left (+) 0  in Format.printf "Part Two: %d\n" res;
  print_endline "hello, galaxies"
