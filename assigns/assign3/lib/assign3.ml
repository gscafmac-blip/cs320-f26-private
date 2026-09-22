let rec remove_key (k: 'a) (l : ('a * 'b) list) (acc: ('a * 'b) list)=
    match l with
    | [] -> List.rev acc
    | (a, _) :: t when a = k -> remove_key k t acc
    | (a, b) :: t -> remove_key k t ((a,b)::acc)

let rec nub (l : ('a * 'b) list) : ('a * 'b) list =
  match l with
  | [] -> []
  | (a, b) :: t -> (a, b) :: nub (remove_key a t [])


let explode (s : string) : char list =
  let rec loop acc i =
    if i = String.length s
    then acc
    else loop (s.[i] :: acc) (i + 1)
  in List.rev (loop [] 0)

let implode (l : char list) : string =
  String.init (List.length l) (List.nth l)

let split_by_ws' (s : string) : string list =
let white = function
  | ' ' | '\n' | '\r' | '\t' -> true
  | _ -> false
  in
  let rec loop (char_list: char list) (acc : char list) (str_list: string list)=
  if acc = [] then
    match char_list with
    | [] -> List.rev str_list
    | c :: t when white c -> loop t [] str_list
    | c :: t -> loop t (c::acc) str_list 
else
  match char_list with
  | [] -> List.rev ((implode (List.rev acc)) :: str_list)
  | c :: t when white c -> loop t [] ((implode (List.rev acc)) :: str_list)
  | c :: t -> loop t (c::acc) str_list
in
loop (explode s) [] []