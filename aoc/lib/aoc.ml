(* I'm actually thinking of implementing a parser combinator just 4 fun, but not today *)
let seq_of_file f = let inch = open_in f
  in let read_line_opt () = try Some(input_line inch) with End_of_file -> None
  in Seq.of_dispenser read_line_opt

let print_string_list s  = print_string "[\n";List.iter (fun s -> print_string s;print_char '\n') s ; print_endline "]"

(* I don't really love this solution so I think I'm gonna roll out my own soon *)
let handle_args = let input_file = ref ""
  in let anon filename = input_file := filename
  in let speclist = []
  in let usage_msg = "missing filename"
  in Arg.parse speclist anon usage_msg; !input_file

module Char = struct 
  include Char
  let is_digit = function 
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'  -> true
    | _ -> false
end

module Fun = struct 
    include Fun 
  let compose f g x = f(g x)
end

module List = struct 
    include List 
  let rec last_opt = function 
      | [] -> None
    | x::[] ->  Some x 
    | _::ys -> last_opt ys
end

module String = struct 
  include String 
  let split_once str c = 
    let (let*) = Option.bind in 
    let* idx = String.index_opt str c
    in Some (String.sub str 0 idx,try String.sub str (idx+1) (String.length str - idx-1) with | Invalid_argument _ -> ""  )

  let strip_prefix prefix str  = 
    if String.starts_with ~prefix:prefix str then 
      String.sub str (String.length prefix) (String.length str - String.length prefix)
      else str
    

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
end



 
