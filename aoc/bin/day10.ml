open Aoc
type point =int*int
type tile = Pipe of point*point | Ground|Start
let tile_of_char x y = function 
  | '|' -> Pipe ((x,y-1), (x,y+1))
  | '-' -> Pipe ((x-1,y), (x+1,y))
  | 'L' -> Pipe ((x,y-1), (x+1,y))
  | 'J' -> Pipe ((x,y-1), (x-1,y))
  | '7' -> Pipe ((x,y+1), (x-1,y))
  | 'F' -> Pipe ((x,y+1), (x+1,y))
  | 'S' -> Start
  | '.' -> Ground
  | _ -> failwith "unrecognized character in input"
let vertical_weight  = function 
  | '|' -> 2
  | '-' -> 0
  | 'L' -> 1
  | 'J' -> -1
  | '7' -> -1
  | 'F' -> 1
  | _ -> 0
let horizontal_weight  = function 
  | '|' -> 0
  | '-' -> 2
  | 'L' -> 1
  | 'J' -> -1
  | '7' -> 1
  | 'F' -> -1
  | _ -> 0

let find_start matrix =
  let sx = ref 0  and sy = ref 0 in 
  for y = 0 to Array.length matrix -1 do 
    for x = 0 to Array.length matrix.(y) -1 do 
      if matrix.(y).(x) == Start then 
        begin
          sx := x;
          sy := y
        end
    done 
  done;
  (!sx,!sy)
;;

(* dist from start - coords *)
type loopnode = Ln of int * (int*int)
module MinOrd  = struct
  type t = loopnode
  let compare a b  = let Ln(x, _) = a and Ln(y, _) = b 
    in
    if x > y then Aoc__Heap.Less else if x = y then Aoc__Heap.Equal else Aoc__Heap.Greater
end

module MinHeap = Aoc__Heap.Make(MinOrd)



let () =
  let matrix = handle_args |> seq_of_file |> Seq.map (Fun.compose Array.of_seq String.to_seq) |> Array.of_seq
  in print_endline "Day10";
  let pipes = Array.mapi (fun y row -> Array.mapi (fun x v -> tile_of_char x y v) row ) matrix 
  in let is_inbound coord = let (x,y) = coord in x >= 0 && x < Array.length pipes.(0) && y >= 0 && y< Array.length pipes
  in let find_nbrs coord = let (x, y) = coord in
       print_endline "finding neighbours";
       match pipes.(y).(x) with 
       | Pipe (o, t) -> [o;t]
       | Start -> let snb = [(x,y+1); (x+1,y); (x-1,y); (x,y-1)] in 
         List.filter (
           function 
           |(nbx,nby) -> let p=pipes.(nby).(nbx) in match p with | Pipe(a,b) -> a = (x,y) || b = (x,y)  | _ -> false
         ) snb
       | _ -> []
  in 
  let sx, sy = find_start pipes in
  Format.printf "Start at %d %d\n" sx sy ;
  let distHeap = MinHeap.pure @@ Ln (0 , (sx, sy))
  in let coordSet = Hashtbl.create 100 
  in let rec follow_path heap =  match MinHeap.pop heap with 
      | (None, _) -> print_endline "heap over"; []
      | (Some Ln(len, coord) , nheap) -> Format.printf "(%d,%d) at %d\n" (fst coord) (snd coord) len ; if is_inbound coord &&  not (Hashtbl.mem coordSet coord) 
        then begin 
          Hashtbl.replace coordSet coord len ;
          let new_heap = List.fold_left (fun acc x ->  MinHeap.push (Ln(len+1,x)) acc) nheap (find_nbrs coord)  in  len :: follow_path new_heap
        end 
        else follow_path  nheap
  in let main_loop = follow_path distHeap  
  in List.last_opt main_loop|>  Option.get |> Format.printf "Day10: %d\n";
  Hashtbl.fold (fun _ len max ->  if len > max then len else max ) coordSet 0 |> Format.printf "Day10: %d\n";
  let  inside_tiles = ref 0 in  
  let check_vert i= let (x, y) = i in let low = Hashtbl.fold (fun (nx,ny) _ count -> if  ny = y && nx < x then (vertical_weight matrix.(ny).(nx)) + count  else count  ) coordSet 0 
    in let high = Hashtbl.fold (fun (nx,ny) _ count -> if  ny = y && nx > x then (vertical_weight matrix.(ny).(nx)) + count  else count  ) coordSet 0 
    in  Format.printf("Hor: %d,%d has %d low and %d high\n") x y low high ;low mod 4 != 0 && high mod 4 != 0
  in let  check_hor i= let (x, y) = i in let low =  Hashtbl.fold (fun (nx,ny) _ count -> if  nx = x && ny< y  then (horizontal_weight matrix.(ny).(nx)) + count  else count  ) coordSet 0 
    and  high  =  Hashtbl.fold (fun (nx,ny) _ count -> if  nx = x && ny> y then (horizontal_weight matrix.(ny).(nx)) + count  else count  ) coordSet 0 
    in  Format.printf("Ver: %d,%d has %d low and %d high\n") x y low high ;low mod 4 != 0 && high mod 4 != 0
    in 
    for y = 0 to Array.length pipes -1 do 
      for x = 0 to Array.length pipes.(y) -1 do 
        if pipes.(y).(x) = Ground then  let ch = check_hor (x,y) and cv = check_vert (x,y)  in 
          if cv  && ch  then begin Format.printf "Inside: %d,%d \n" x y; inside_tiles := !inside_tiles + 1 end
      done 
    done; Format.printf "Part 2: %d" !inside_tiles

  (*
  let print_pipe x y= function 
    | Pipe (_,_) -> if (Hashtbl.mem coordSet (x,y)) then print_int (Hashtbl.find coordSet (x,y)) else print_char matrix.(y).(x)
      | _ -> print_char matrix.(y).(x) in 
  Array.iteri (fun y r -> Array.iteri (fun x v -> print_pipe  x y v ) r; print_char '\n') pipes

  *)
