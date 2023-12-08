open Aoc
let assoc_vals = [
  ('A' , 14);
  ('K' , 13);
  ('Q' , 12);
  ('J' , 11);
  ('T' , 10);
  ('9' , 9);
  ('8' , 8);
  ('7' , 7);
  ('6' , 6);
  ('5' , 5);
  ('4' , 4);
  ('3' , 3);
  ('2' , 2)];;
let assoc_joks = [
  ('A' , 14);
  ('K' , 13);
  ('Q' , 12);
  ('J' , 1);
  ('T' , 10);
  ('9' , 9);
  ('8' , 8);
  ('7' , 7);
  ('6' , 6);
  ('5' , 5);
  ('4' , 4);
  ('3' , 3);
  ('2' , 2)] ;;

let _compare_face a b = 
  (*lol da cambiare*)
  compare (List.assoc a assoc_vals) (List.assoc b assoc_vals)
let compare_joks a b = 
  (*lol da cambiare*)
  compare (List.assoc a assoc_joks) (List.assoc b assoc_joks)

let _rank hand = 
  let map = Hashtbl.create 5 in 
  String.iter (fun a -> Hashtbl.add map a (
      Option.value (Option.map ((+)1) (Hashtbl.find_opt map a)) ~default:0
    ) ) hand;
  let score = Hashtbl.fold (fun _ v acc ->  v*v + acc) map 0 in 
 (* Format.printf "Score of %s: %d\n" hand score ;*)
  score

let rank_joks hand = 
  let map = Hashtbl.create 5 in 
  String.iter (fun a -> Hashtbl.replace map a (
      1+ (Option.value  (Hashtbl.find_opt map a) ~default:0)
    ) ) hand;
  let joks = Hashtbl.find_opt map  'J' in 
  Hashtbl.remove map 'J'  ;
  let(_, lg) = Hashtbl.fold (fun k v (acc, c) -> if v > acc then (v, k) else (acc, c)  ) map (0,'J') in 
  let bo () = match  joks with
    | Some value ->  Hashtbl.replace map lg ((Option.value (Hashtbl.find_opt map lg ) ~default:0) +value)
    | None -> ()
  in 
  bo ();
  (*Hashtbl.iter (fun k v -> Format.printf "%c->%d\n"k v) map;*)
  let score = Hashtbl.fold (fun _ v acc ->  v*v + acc) map 0 in 
  (*Format.printf "Score of %s: %d\n" hand score ;*)
  score


let compare_hands a b =
  let ranka = rank_joks a and rankb = rank_joks b in
  if ranka != rankb then 
    compare ranka rankb
  else 
    let rec comp sa sb = 
      match (sa () , sb ())  with 
      | (Seq.Cons(x,xs), Seq.Cons(y,ys)) when x = y  -> comp xs ys
      | (Seq.Cons(x,_), Seq.Cons(y,_) ) -> compare_joks x y 
      | _ -> failwith "hands of different lenght"
    in comp (String.to_seq a) (String.to_seq b)

let sort_by_rank = List.sort (fun (hand, _) (hand2,_) -> compare_hands hand hand2)

let () = let lines = handle_args |> seq_of_file |> List.of_seq in 
  let hands = List.map (Fun.flip String.split_once ' '  ) lines |> List.filter Option.is_some |> List.map Option.get
  in 
  print_string_list lines;
  let sorted_hands = sort_by_rank hands 
  in let rec count acc = function 
      |[] -> 0
      | (_,bet)::ys -> acc * (int_of_string bet) + count (acc+1) ys
  in let res =  count 1 sorted_hands
  in 
  print_endline "Day 07";
  print_string_list (fst @@ List.split sorted_hands);
  Format.printf "Crazy sum: %d\n" res;

