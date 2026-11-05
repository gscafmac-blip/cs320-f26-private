open Utils

module Type = struct
  type t =
    | Nat
    | Arr of t * t


  (* this will just return the typing of some type explicitly declared in the string*)
  let rec of_sexpr_opt (e : Sexpr.t) : t option =
    match e with
    | Sexpr.Atom "Nat" -> Some Nat
    | Sexpr.List [Sexpr.Atom "->"; a; b] -> begin
      match of_sexpr_opt a, of_sexpr_opt b with
      | Some a', Some b' -> Some (Arr (a', b'))
      | _ -> None
    end
    | _ -> None
end

module Expr = struct
  type t =
    | Var of string
    | Fun of string * Type.t * t
    | App of t * t
    | Let of string * t * t
    | Zero
    | Succ of t
    | Rec_nat of t * t * t


  let num_to_succ n =
    let rec go n acc =
      match n with
      | x when x = 0 -> acc
      | _ -> go (n-1) (Succ acc)
    in go n Zero

  let of_sexpr_opt se = 
    let rec go se =
      match se with

      | Sexpr.Atom a -> begin
        match int_of_string_opt a with
        | Some n -> Some (num_to_succ n) (* if its a number *)
        | None -> Some (Var a)           (* if the atom is just a var *)
      end

      | Sexpr.List l -> begin
        match l with
        | [Sexpr.Atom "lambda"; Sexpr.Atom x; ty; body] -> begin
          match go body, (Type.of_sexpr_opt (ty)) with
          | Some body', Some ty' -> Some (Fun (x, ty', body')) (* im gonna use the of_sexpr from Type.t above *)
          | _, _ -> None
        end

        (* sub e1 for x in e2 *)
        | [Sexpr.Atom "let"; Sexpr.Atom x; e1; e2] -> begin
          match go e1, go e2 with
          | Some e1', Some e2' -> Some(Let (x, e1', e2'))
          | _ -> None
        end

         (* (succ e) *)
        | [Sexpr.Atom "add1"; e] -> begin
            match go e with
            | Some e' -> Some (Succ e')
            | None -> None
          end

        | [Sexpr.Atom "rec-Nat"; a; b; c] -> begin
            match go a, go b, go c with
            | Some a', Some b', Some c' -> Some (Rec_nat (a', b', c'))
            | _ -> None
          end

        | [f; x] -> begin
            match go f, go x with
            | Some f', Some x' -> Some (App (f', x'))
            | _ -> None
          end

      | _ -> None
      end
      in go se

  let of_string_res s =
    match Sexpr.of_string_opt s with
    | Some se -> Option.to_result ~none:"syntax error" (of_sexpr_opt se)
    | None -> Error "syntax error"

  let of_string s =
    match of_string_res s with
    | Ok e -> e
    | Error _ -> raise (Failure "Expr.of_string")

  let subst (e' : t) (x : string) (e : t) : t = ignore (e', x, e); assert false (* TODO *)

    let rec subst v y e =
      match e with
      | Fun (x, ty, body) -> 
        if String.equal x y then e
        else Fun (x, ty, subst v y body)
      | App (a, b) -> App ((subst v y a), (subst v y b))
      | Var a -> if (String.equal a y) then v else e
      | Zero -> Zero
      | Succ a -> Succ (subst v y a)
      | Let (x, e1, e2) -> begin
        let e1' = subst v y e1 in
        if String.equal x y then Let (x, e1', e2)
        else Let (x, e1', subst v y e2)
      end
      | Rec_nat (a, b, c ) -> Rec_nat (subst v y a, subst v y b, subst v y c)
end

module Ctxt = struct
  module M = Map.Make(String)
  type t = Type.t M.t

  (* empty context

     Ctxt.empty ≡ ∅ ≡ {}
   *)
  let empty = M.empty

  (* Determine declared type for a variable in the context

     Ctxt.find_opt x Γ = { Some τ    if (x : τ) ∈ Γ
                         { None      otherwise
   *)
  let find_opt : string -> t -> Type.t option = M.find_opt

  (* Add a declartion to the context

     Ctxt.add x τ Γ ≡ Γ, x : τ
   *)
  let add : string -> Type.t -> t -> t = M.add
end

let ( let* ) = Result.bind

let rec type_of (gamma : Ctxt.t) (e : Expr.t) : (Type.t, string) result =
  match e with
  | Expr.Var x -> begin
      match Ctxt.find_opt x gamma with
      | Some ty -> Ok ty
      | None -> Error (Printf.sprintf "Type of %s not in context" x)
    end

  | Expr.Fun (x, ty, body) ->
      let* ty' = type_of (Ctxt.add x ty gamma) body in
      Ok (Type.Arr (ty, ty'))

  | Expr.App (e1, e2) -> begin
      let* t1 = type_of gamma e1 in
      let* t2 = type_of gamma e2 in
      match t1 with
      | Type.Arr (dom, cod) when dom = t2 -> Ok cod
      | _ -> Error "bad app"
    end

  | Expr.Let (x, e1, e2) ->
      let* t1 = type_of gamma e1 in
      type_of (Ctxt.add x t1 gamma) e2

  | Expr.Zero -> Ok Type.Nat

  | Expr.Succ e' -> begin
      let* t = type_of gamma e' in
      match t with
      | Type.Nat -> Ok Type.Nat
      | _ -> Error "add1 needs Nat expr"
    end

  | Expr.Rec_nat (n, base, step) -> begin
      let* tn = type_of gamma n in
      let* tb = type_of gamma base in
      let* ts = type_of gamma step in
      if tn <> Type.Nat then Error "type of first arg must be nat"
      else if ts <> Type.Arr (Type.Nat, Type.Arr (tb, tb)) then
        Error "wrong arr type"
      else Ok tb
    end

let rec eval (e : Expr.t) : Expr.t =
  match e with
  | Expr.Var x -> failwith ("unbound variable" ^ x)

  | Expr.Fun _ 
  | Expr.Zero -> e

  | Expr.Succ e' -> Expr.Succ (eval e')

  | Expr.App (e1, e2) -> begin
      match eval e1 with
      | Expr.Fun (x, _, body) -> eval (Expr.subst (eval e2) x body)
      | _ -> failwith "applying non-function"
    end

  | Expr.Let (x, e1, e2) -> eval (Expr.subst (eval e1) x e2)

  | Expr.Rec_nat (n, base, step) -> begin
      match eval n with
      | Expr.Zero -> eval base
      | Expr.Succ m ->
          eval (Expr.App (Expr.App (step, m), Expr.Rec_nat (m, base, step)))
      | _ -> failwith "rec-Nat first arg needs to be num"
    end

let string_of_value e =
  let rec int_of_value_opt : Expr.t -> int option = function
    | Zero -> Some 0
    | Succ n ->
       begin
         match int_of_value_opt n with
         | Some n -> Some (n + 1)
         | None -> None
       end
    | _ -> None
  in
  match int_of_value_opt e with
  | Some n -> string_of_int n
  | None -> "<fun>"

let interp (s : string) : unit =
  match Expr.of_string_res s with
  | Ok e ->
     begin
       match type_of Ctxt.empty e with
       | Ok _ -> Format.printf "Success: %s" (string_of_value (eval e))
       | Error e -> Format.eprintf "Error: %s" e
     end
  | Error e -> Format.eprintf "Error: %s" e
