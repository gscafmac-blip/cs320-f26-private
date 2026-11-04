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

        (* (rec_nat e0 e1 e2) -- adjust to your argument order *)
        | [Sexpr.Atom "rec-Nat"; a; b; c] -> begin
            match go a, go b, go c with
            | Some a', Some b', Some c' -> Some (Rec_nat (a', b', c'))
            | _ -> None
          end

        (* application: (f x) *)
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

let rec type_of (gamma: Ctxt.t) (e : Expr.t) : (Type.t, string) result =
  ignore (gamma, e); assert false (* TODO *)

let rec eval (e : Expr.t) : Expr.t =
  ignore e; assert false (* TODO *)

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
