(* I'm actually thinking of implementing a parser combinator just 4 fun, but not today *)
let seq_of_file f = let inch = open_in f
  in let read_line_opt () = try Some(input_line inch) with End_of_file -> None
  in Seq.of_dispenser read_line_opt

let print_string_list s  = print_char '[';List.iter (fun s -> print_string s) s ; print_endline "]"

module Char = struct 
  include Char
  let is_ascii_number = function 
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'  -> true
    | _ -> false
end

module String = struct 
  include String 
  let split_once str c = 
    let (let*) = Option.bind in 
    let* idx = String.index_opt str c
    in Some (String.sub str 0 idx,try String.sub str (idx+1) (String.length str - idx-1) with | Invalid_argument _ -> ""  )

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
