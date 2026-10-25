open Utils

module Sexpr = Sexpr

module Expr = struct
  type t =
    | Fun of string * t
    | App of t * t
    | Var of string

  let to_string e =
    let rec go = function
      | Fun (x, e) -> Format.sprintf "(lambda %s %s)" x (go e)
      | App (e1, e2) -> Format.sprintf "(%s %s)" (go e1) (go e2)
      | Var x -> x
    in go e

  let ( let* ) = Option.bind

  let rec of_sexpr_opt (se: Sexpr.t) : t option =
    let rec go se =
      match se with
      | Sexpr.Atom a -> Some (Var a)
      | Sexpr.List l -> begin
        match l with
        | [Sexpr.Atom "lambda"; Sexpr.Atom x; body] -> begin
          match go body with
          | Some e -> Some (Fun (x, e))
          | None -> None
        end
        (*application case*)
        | [f; x] -> begin
          match ((go f), (go x)) with
          | (Some f_p, Some x_p) -> Some (App (f_p, x_p))
          | _ -> None
        end
      | _ -> None
      end
      in go se

  let of_string_opt s =
    let* se = Sexpr.of_string_opt s in
    of_sexpr_opt se

  let of_string s =
    match of_string_opt s with
    | Some e -> e
    | None -> raise (Failure "Expr.of_string")

  let rec fv : t -> VarSet.t = function
    | Fun (x, e) -> VarSet.remove x (fv e)
    | App (e1, e2) -> VarSet.union (fv e1) (fv e2)
    | Var x -> VarSet.singleton x
  
let rec free_vars e =
  match e with
  | Var a -> [a]
  | App (a, b) -> (free_vars a) @ (free_vars b)
  | Fun (x, e) -> List.filter (fun v -> not (String.equal v x)) (free_vars e) (* go through and grab non bound vals*)
  
  let rec subst v y e =
      match e with
      | Fun (x, e) -> begin
        if (String.equal x y) then Fun (x, e)

        else if (List.mem x (free_vars v)) (* x is a free variable of v, i looked up best dict alternative in ocaml*) then
        let z = Utils.gensym() in
        let e_renamed = subst (Var z) x e in
        Fun (z, subst v y e_renamed) else 
        Fun (x, subst v y e)
      end
      | App (a, b) -> App ((subst v y a), (subst v y b))
      | Var a -> if (String.equal a y) then v else Var a


  let rec equal (e1 : t) (e2 : t) : bool = 
    match (e1, e2) with
    | Var a, Var b -> String.equal a b
    | App(a, b), App(c, d) -> equal a c && equal b d
    | Fun(x1, body1), Fun(x2, body2) -> equal body1 (subst (Var x1) x2 body2)
    | _ -> false

end
let step e = ignore e; assert false


let multistep =
  let rec go e =
    match step e with
    | Some e -> go e
    | None -> e
  in go
(*
let interp s =
  match Expr.of_string_opt s with
  | None -> print_endline "parse error"
  | Some e ->
     match multistep e with
     | e -> e |> Expr.to_string |> print_endline

let expr_of_int n = ignore n; assert false

let int_of_expr_opt e = ignore e; assert false

let int_of_expr e =
  match int_of_expr_opt e with
  | Some n -> n
  | None -> raise (Failure "int_of_expr")

let suc : Expr.t = Expr.Var "x" (* TODO *)
let add : Expr.t = Expr.Var "x" (* TODO *)
*)