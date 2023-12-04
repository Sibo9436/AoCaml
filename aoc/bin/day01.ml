let _testf = "inputs/day01_test.txt"
let _inputf = "inputs/day01.txt"
open Aoc
let words  = [
  "one" , "1";
  "two" , "2";
  "three" , "3";
  "four" , "4";
  "five" , "5";
  "six" , "6";
  "seven" , "7";
  "eight" , "8";
  "nine" , "9"
]

let find_substr str sub = 
  let rec loop start slen =
    if (slen + start) > String.length str then []
    else 
      match String.sub str start slen with
      | m when m = sub -> (start,slen)::loop (start+1)(slen)
      | _ -> loop (start +1 ) slen
  in loop 0 (String.length sub)

let string_rev s =
  let len = String.length s in
  String.init len (fun i -> s.[len - 1 - i])
let switch_numbers str = let result = 
  List.fold_left (
    fun acc (w,n)  ->  let ins = (String.make (String.length w - 2) ' ')^n^" " in 
      List.fold_left ( fun akk (start, slen) -> (*print_endline @@ akk^"-" ^ ins ^"-"^w;*)
                        if start = 0 && (start + slen )>= (String.length akk) then ins
                        else if start = 0 then ins ^ String.sub akk (slen ) (String.length str - slen )
                        else if start+slen >= (String.length akk) then String.sub akk 0 (start ) ^ ins
                        else  (String.sub akk 0 ( start )) ^ ins  ^ (String.sub akk (start + slen) (String.length str -slen- start ) ) 
                      )    acc (find_substr str w ) 

  ) str  words
in print_endline result ; result

let () = let file  = open_in _inputf
  in let read_line_opt () = try Some(input_line file) with End_of_file -> None
  in
  let find_pair x  = let just_num =  String.fold_left (fun acc c -> if String.length acc = 0 && Char.is_digit c then  String.make 1 c   else acc ) ""
    in just_num x  ^ just_num (string_rev x)
  in 
  let rec _first = function 
    | None -> close_in file ; []
    | Some l -> print_endline l ;( int_of_string_opt@@ find_pair l):: (_first @@ read_line_opt ())
  in let rec _second = function 
    | None -> close_in file ; []
    | Some l -> print_endline l ;( int_of_string_opt@@ find_pair (switch_numbers l )):: (_second @@ read_line_opt ())
  in let res =  _second @@ read_line_opt ()
  in print_int @@ List.fold_left (fun acc -> function |None -> acc | Some v -> v+acc) 0  res
