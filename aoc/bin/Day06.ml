open Aoc
let parse_to_list s = String.split_once s ':'|>Option.get|>snd|>String.split_on_char ' '  |> List.filter ((<>)String.empty) |> List.map int_of_string
let bin_search time record = 
  let cap = record/2 +1 in 
  let jump = int_of_float @@ Float.log2 (float_of_int cap) in 
  let rec lin start en = 
    if start > en then 0 else (*should be impossible*)
    if start * (time -start) > record then start 
    else lin (start+1) en
  in 
  let rec search pos = 
    if pos > cap then 0 else
    if pos * (time - pos ) > record then lin (pos-jump) pos
    else search (pos+jump)
  in let pos = (search jump) -1 in 
  (time - 1) - (pos *2)
;;
let better_count time record = 
  let rec count counter = 
    if counter > time then 0
    else if counter * (time - counter)> record then counter
    else count(counter+1)
  in let losses = (count 0 ) -1 in 
  (time-1) - (losses *2 ) 
;;
let timer f = let s = Sys.time  () in f(); let diff = Sys.time() -. s  in Format.printf "Time for exec: %f\n" diff;Format.print_flush();;

let () = 
  let lines = handle_args |> seq_of_file 
  in let parse_next () = match lines () with 
      | Seq.Nil -> failwith "Lines finished early"
      | Seq.Cons(x, _) -> parse_to_list x 
  in 
  let times = parse_next () in 
  let records = parse_next () in 
  print_endline "Part one";
  print_int @@ List.fold_left2 (fun acc a b ->acc *  better_count a b ) 1  times records; print_endline "\nDone";
  print_endline "Part two";
  let bigtime = List.fold_left(fun acc s -> acc^(string_of_int s)) "" times|> int_of_string in 
  let bigrecord = List.fold_left(fun acc s -> acc^(string_of_int s)) "" records|> int_of_string in 
  timer ( fun () ->print_int @@ better_count bigtime bigrecord; print_endline "\nDone");
  timer ( fun () ->print_int @@ bin_search bigtime bigrecord; print_endline "\nDone");

