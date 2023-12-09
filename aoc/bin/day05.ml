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
(* joins range a (a->a') and b(a'->b)  into range c(a->b) 
let apply_range a b = 
let rec new_range  = function 
  |Mapping (dst, src, len)   -> 
  in 
  List.map (

    new_range 
    ) a |> List.flatten

*)
      
let rev_mapping maps value = 
  let rec m li v = match li with 
    | [] -> v  
  | Mapping(dest, source, len)::rest -> 
      if Int64.compare v dest >= 0 && Int64.compare  v  (Int64.add dest  len) < 0 then 
      Int64.add source (Int64.sub v dest)
      else  m  rest v 
  in List.fold_right m  maps value 
    
      

let part_two seeds maps = 
  let in_seed_range value = 
  let rec check = function 
  | [] -> false
  | (start,len)::rest -> if Int64.compare value start >= 0 && Int64.compare value (Int64.add start len) < 0 then true else check rest
in check seeds
    in 
let rec work s = match s () with 
    | Seq.Nil -> failwith "whatthehell"
    |Seq.Cons(v,rest) -> if in_seed_range (rev_mapping maps v) then v else work rest
  in 
  work (Seq.map (Int64.of_int ) @@Seq.ints 0 )

;;

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
    let rec seeded = function 
      | [] -> []
    | a::b::rest -> (a,b)::(seeded rest)
    | _ -> failwith "Uneven number of seeds"
    in let (seeds, maps) = res in 
  let newseedlist = seeded seeds in 
    print_endline "Part two:";
    print_int @@ Int64.to_int @@ part_two newseedlist maps ; print_char '\n'  ;
    print_endline "Done";








