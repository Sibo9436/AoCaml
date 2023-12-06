open Aoc
let parse_seeds l = 
  snd (Option.get @@ String.split_once l ':') 
  |> String.trim |> String.split_on_char ' ' 
  |> List.map (Fun.compose Int64.of_string String.trim)
;; (* I've seen this in other code is it just to cp to utop? *)

(* dest_st source_st range *)
type mapping = Mapping of int64 * int64 * int64
let print_mapping = function Mapping (a,b,c) -> Format.printf "(%s %s %s)" (Int64.to_string a) (Int64.to_string b)(Int64.to_string c)
(* Non ho ancora deciso se preferisco |> o @@ entrambi hanno i loro momenti *)
let parse_mapping s = s |> String.trim |> String.split_on_char ' ' 
                  |> List.map (Fun.compose Int64.of_string String.trim)
                  |> function | a::b::c::[] -> Mapping (a, b, c) | _ -> failwith "No"
;;

(* in better code we should use options or results but we needn't here *)
let parse_map s = match String.split_on_char '\n' s with  
    | [] -> failwith ("mapping should not be empty "^s)
    | _::[] -> failwith ("mapping should have more than one line "^s)
  | _::rest -> List.map parse_mapping rest


let split_on_string pat s = 
  let subs = String.find_substr s pat in 
  let rec get start = function 
  | [] -> [String.sub s start (String.length s - start)] 
  | (st,sz)::rest -> String.sub s start (st -start) :: get (st+sz)rest
in get 0 subs

let map_seed maps seed = 
  let rec m v = function 
    | [] -> v  
  | Mapping(dest, source, len)::rest -> 
      if Int64.compare v source >= 0 && Int64.compare  v  (Int64.add source  len) < 0 then 
      Int64.add dest (Int64.sub v source)
      else  m v rest
  in List.fold_left m seed maps
    
    
  
let part_one (seeds, maps) = 
  let mapped = List.map (map_seed maps) seeds  in print_string_list @@ List.map (Int64.to_string) mapped;
  List.fold_left (fun acc x -> if compare x acc < 0  then x else acc) Int64.max_int mapped

  (* l'idea adesso è di mappare i range *)
(*
let apply_range a b = 
  let rec apply_single_range  = 
    function Mapping(d, s, range) -> 
      List.map (
      function Mapping (dd,ds,dr) ->
      let endd = Int64.add d range and dend = Int64.add ds dr in 
        if Int64.compare d ds>= 0 
        then if Int64.compare 
        else if Int64.compare (Int64.add d range) (Int64.add ds  dr)<= 0
  then 
        Some Mapping()::apply_single_range()
        else None

    ) m 
      
      

let part_two (seeds, maps) = 


;;
*)

let () = let input  = handle_args 
  |> seq_of_file |>List.of_seq |> String.concat "\n"   in
  (* avevo pensato di usare la libreria Str per le regex ma non è chiaro se 
     sia std o meno e io per sicurezza sego  *)
  let inblocks = split_on_string "\n\n" (String.trim input)
  in let in_opt  = match inblocks with 
    | [] -> None
   | head ::rest -> Some (parse_seeds head, List.map (parse_map) rest)
  in match in_opt with | None -> print_endline "NOooooo"
  | Some res -> List.iter (List.iter print_mapping) (snd res) ;
    print_endline "Part one:";
    print_int @@ Int64.to_int @@ part_one res  ;
    print_endline "Done";
   (* 
    let rec seeded = function 
      | [] -> []
    | a::b::rest -> (a,b)::(seeded rest)
    | _ -> failwith "Uneven number of seeds"
    in let (seeds, maps) = res in 
  let newseedlist = seeded seeds in 
    print_endline "Part two:";
    print_int @@ Int64.to_int @@ part_one (newseedlist, maps)  ;
    print_endline "Done";
  *)








