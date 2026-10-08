type switch =
  | Pos_zero
  | Neg_zero
  | Pos
  | Neg
  | First

(* switch: state of the last element read *)
let group (l : int list) : int list list option =
  let rec loop l switch temp acc =
    match l with
    | [] -> begin
        match switch with
        | Pos | Neg -> Some (List.rev (List.rev temp :: acc))
        | First | Pos_zero | Neg_zero -> None (* empty list or end in 0*)
      end
    | 0 :: t -> begin
        match switch with
        | Pos -> loop t Pos_zero [] (List.rev temp :: acc)
        | Neg -> loop t Neg_zero [] (List.rev temp :: acc)
        | First | Pos_zero | Neg_zero -> None   (* leading or 0 in row*)
      end
    | i :: t when i < 0 -> begin
        match switch with
        | Neg -> loop t Neg (i :: temp) acc
        | First | Pos_zero -> loop t Neg [i] acc
        | Pos | Neg_zero -> None   (* sign change or 0 between same signs *)
      end
    | i :: t -> begin
        match switch with
        | Pos -> loop t Pos (i :: temp) acc
        | First | Neg_zero -> loop t Pos [i] acc
        | Neg | Pos_zero -> None
      end
  in
  loop l First [] []
        
  

type 'a rtree = Node of 'a * 'a rtree list

let rec unzip l =
  match l with
  | [] -> ([], [])
  | (a, b) :: rest ->
    let aas, bbs = unzip rest in
    (a :: aas, b :: bbs)

let rec split (t : ('a * 'b) rtree) : 'a rtree * 'b rtree =
  match t with
  | Node ((a, b), children) ->
    let lefts, rights = unzip (List.map split children) in (* list.split goes through the list of children left and 
    right trees we have created*)
    (Node (a, lefts), Node(b, rights))


let prefix_map (f : 'a list -> 'b option) (l : 'a list) : ('b * 'a list) option =
  let rec loop l pre =
    match (f (List.rev pre)) with
    | None -> begin
      match l with 
      | [] -> None
      | h :: t -> loop t (h :: pre)
    end
    | Some b -> Some (b, l)
  in
  loop l []

  

let apply_cycle (f : ('a -> 'a) list) (n : int) (x : 'a) : 'a =
  let rec loop funcs n x =
    if (not (n = 0)) then
      match funcs with
      | h :: rest -> loop rest (n-1) (h x)
      | [] -> loop f n x
    else x
  in  
  loop f n x

let walks (f : 'a -> 'a -> bool) (n : int) (ps : (('a -> 'a) * 'a) list) : 'a list =
  let _ = f, n, ps in
  assert false