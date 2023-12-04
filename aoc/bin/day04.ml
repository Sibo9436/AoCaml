open Aoc
type card = Card of int* int list * int list
(*
let winning c =   let Card(_, w, _ ) = c  in w
let mine c =   let Card(_, _, m ) = c  in m
let id c =   let Card(m, _, _ ) = c  in m
*)
let _print_card card = let Card(id,win,mine) = card in  print_string "Card ("; print_int id; List.iter (fun c -> print_char ' ';print_int c) win;
  print_string " | " ;
  List.iter (fun c -> print_char ' ';print_int c) mine; print_char ')'

(* again a parser combinator would make short work of this... *)
let parse_values s = print_endline @@"parsing values "^s;
  let compose f g x = f(g x) in
  String.split_on_char ' ' (String.trim s)|> List.filter ((<>)String.empty) |> List.map (compose int_of_string String.trim)

let parse_id s = print_endline @@ "parsing id " ^ s;
  let _,idstr = Option.get @@ String.split_once s ' '
  in print_endline idstr ;flush_all (); int_of_string (String.trim idstr)

let parse_card s = print_endline s;flush stdout;
  String.split_once s ':' |> function 
  |None -> failwith "no" 
  | Some (c,vals) -> let w, m = Option.get @@ String.split_once vals '|' in
    Card (parse_id c,parse_values w ,parse_values  m)

    (* absolutely hideous *)
    let count_winning c = let Card (_,w,m)  = c in 
  let winmap =List.map (fun x -> (x,0)) w|> List.to_seq  |> Hashtbl.of_seq in 
  (* I'm not loving this side effect riddled way *)
  let rec countwinners = function 
    | [] -> ()
    | h::rest -> let () = countwinners rest in if Hashtbl.mem winmap h then Hashtbl.replace  winmap h ((Hashtbl.find winmap h) +1) 
      else ()
  in countwinners m;  Hashtbl.fold (fun _ b acc -> acc + b ) winmap 0 

let wins c = int_of_float @@2.0**(float_of_int (count_winning c) -. 1.0)

let part_two list =
  let tbl = Seq.init (List.length list) (fun x -> (x+1,1)) |> Hashtbl.of_seq
  in 
  let rec helper = 
  function 
  | [] -> ()
  | Card(idx, _, _) as card::rest -> 
      let st = idx+1 and winners = count_winning card in 
        for i = st to st+winners-1 do 
          Hashtbl.replace tbl i ((Hashtbl.find tbl idx)+ (Hashtbl.find tbl i) )
        done ; helper rest
  in helper list; Hashtbl.fold (fun _ v acc -> v +acc) tbl 0



let () = 
  let file = handle_args in 
  let lines = seq_of_file file |> Seq.memoize in
  print_endline "Day04";
  print_endline "Part 1";
  Seq.map parse_card lines |> Seq.map wins |> Seq.fold_left (+) 0 |> print_int ; print_endline "";
  Seq.map parse_card lines |> List.of_seq |>  part_two |> print_int ; print_endline "";

