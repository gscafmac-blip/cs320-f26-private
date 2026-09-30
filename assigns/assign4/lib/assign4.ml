
let is_ws c = c = " " || c = "\n" || c = "\r" || c = "\t" || c = "\012"


let split_by_ws (s : string) : string list =
  let str_len = String.length s in

  let rec loop (s: string) (rtn_lst: string list) (front: int) (len: int) =
    if (front + len) > str_len then
      let word = String.sub s front (str_len - front) in
      List.rev (word :: rtn_lst)
    else

    let next_char = String.sub s (front + len - 1) 1 in
    let word = String.sub s front (len - 1) in

    if is_ws next_char then
      if not (word = "") then loop s (word :: rtn_lst) (front + len) 1
      else loop s rtn_lst (front + len) 1
    else
      loop s rtn_lst front (len + 1)
    in loop s [] 0 1




type dir = N | S | E | W

let dist (dirs : dir list) : float =
  let rec loop (dirs: dir list) (horizontal: int) (vertical: int) =
    match dirs with
    | [] -> sqrt (float_of_int (horizontal * horizontal + vertical * vertical))
    | N :: t -> loop t horizontal (vertical + 1)
    | S :: t -> loop t horizontal (vertical - 1)
    | E :: t -> loop t (horizontal + 1) vertical
    | W :: t -> loop t (horizontal - 1) vertical
  in loop dirs 0 0





    type int_or_string
  = Int of int
  | String of string

type int_list_or_string_list
  = Int_list of int list
  | String_list of string list


(* found info on algabreic data types in the textbook because we didnt get to it last thursday*)
let convert (l : int_or_string list) : int_list_or_string_list list =
  let rec loop (l : int_or_string list) (ints: int list) (strings: string list) (rtn_list: int_list_or_string_list list) =
    match l with
    | [] -> List.rev (String_list (List.rev strings) :: Int_list (List.rev ints) :: rtn_list) (* just tack on the left overs at end*)
    | Int i :: t when (strings = []) -> loop t (i :: ints) strings rtn_list
    | Int i :: t -> loop t (i :: ints) [] (String_list (List.rev strings) :: rtn_list)
    | String s :: t when (ints = []) -> loop t ints (s :: strings) rtn_list
    | String s :: t -> loop t [] (s :: strings) (Int_list (List.rev ints) :: rtn_list)
  in loop l [] [] []






type 'a tree
  = Empty
  | Node of 'a * 'a tree * 'a tree

let rec insert (x : 'a) (t : 'a tree) : 'a tree =
  let rec loop (x: 'a) (t: 'a tree) =
    match t with
    | Empty -> Node (x, Empty, Empty)
    | Node (a, left, right) when (x <= a) -> Node (a, loop x left, right)
    | Node (a, left, right) -> Node (a, left, loop x right)
  in loop x t
(* this way we just pass nodes there old selves, but hand off the x till it fits in,,
I am way too lazy this week to do tail recursive...*)

let rec flatten (t : 'a tree) : 'a list =
 let rec loop t (acc: 'a list) =
  match t with
  | Empty -> acc
  | Node (a, left, right) -> loop left (a :: loop right acc) (* here we know everything on right will be greater than
  anything down left, so we cons a on and work on left after we have3 gone all the way down the right*)
 in loop t []
 

let rec sort (l : 'a list) : 'a list =
  let _ = l in
  assert false
