let rec remove_key (k: 'a) (l : ('a * 'b) list) =
    match l with
    | [] -> []
    | (a, _) :: t when a = k -> remove_key k t
    | (a, b) :: t -> (a, b) :: remove_key k t

let rec nub (l : ('a * 'b) list) : ('a * 'b) list =
  match l with
  | [] -> []
  | (a, b) :: t -> (a, b) :: nub (remove_key a t)


(*
let split_by_ws' (s : string) : string list =
  let _ = s in
  assert false
*)