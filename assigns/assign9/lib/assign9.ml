include Utils
module Sexpr = Sexpr

let ( let* ) = Option.bind

module Bop = struct
  type t =
    | Add | Sub | Mul
    | Eq | Neq | Lt | Lte | Gt | Gte

  let of_sexpr_opt (se : Sexpr.t) : t option =
    match se with
    | Atom w ->
       begin
         match w with
         | "+" -> Some Add
         | "-" -> Some Sub
         | "*" -> Some Mul
         | "=" -> Some Eq
         | "<>" -> Some Neq
         | "<" -> Some Lt
         | "<=" -> Some Lte
         | ">" -> Some Gt
         | ">=" -> Some Gte
         | _ -> None
       end
    | _ -> None
end

module Type = struct
  type t =
    | Int
    | Bool
    | Fun of t * t
    | List of t

  let rec of_sexpr_opt (se : Sexpr.t) : t option =
    match se with
    | Atom "int" -> Some Int
    | Atom "bool" -> Some Bool
    | List [Atom "->"; t1; t2] ->
       let* t1 = of_sexpr_opt t1 in
       let* t2 = of_sexpr_opt t2 in
       Some (Fun (t1, t2))
    | List [Atom "list"; t] ->
       let* t = of_sexpr_opt t in
       Some (List t)
    | _ -> None

  let rec to_string : t -> string = function
    | Int -> "int"
    | Bool -> "bool"
    | Fun (t1, t2) -> Format.sprintf "(-> %s %s)" (to_string t1) (to_string t2)
    | List t -> Format.sprintf "(list %s)" (to_string t)
end

module Pattern = struct
  type t =
    | Wildcard
    | Int of int
    | Bool of bool
    | Var of string
    | Nil
    | Cons of t * t

  let rec of_sexpr_opt (se : Sexpr.t) : t option =
    match se with
    | Atom "_" -> Some Wildcard
    | Atom "[]" -> Some Nil
    | Atom "true" -> Some (Bool true)
    | Atom "false" -> Some (Bool false)
    | Atom w when is_int w -> Some (Int (int_of_string w))
    | Atom w when is_var w -> Some (Var w)
    | List [Atom "::"; p1; p2] ->
       let* p1 = of_sexpr_opt p1 in
       let* p2 = of_sexpr_opt p2 in
       Some (Cons (p1, p2))
    | _ -> None
end

module Expr = struct
  type t =
    | Int of int
    | Bool of bool
    | Nil of Type.t
    | Cons of t * t
    | Var of string
    | Bop of Bop.t * t * t
    | Fun of string * Type.t * t
    | App of t * t
    | If of t * t * t
    | Let of string * Type.t * t * t
    | Match of t * (Pattern.t * t) list

  let rec of_sexpr_opt (se : Sexpr.t) : t option =
    let arm_of_sexpr_opt (se : Sexpr.t) : (Pattern.t * t) option =
      match se with
      | List [p; e] ->
         let* p = Pattern.of_sexpr_opt p in
         let* e = of_sexpr_opt e in
         Some (p, e)
      | _ -> None
    in
    let rec arms_of_sexpr_opt (se : Sexpr.t) : (Pattern.t * t) list option =
      match se with
      | List [] -> None
      | List [arm] ->
         let* arm = arm_of_sexpr_opt arm in
         Some [arm]
      | List (arm :: arms) ->
         let* arm = arm_of_sexpr_opt arm in
         let* arms = arms_of_sexpr_opt (List arms) in
         Some (arm :: arms)
      | _ -> None
    in
    match se with
    | Sexpr.Atom w ->
       begin
         match w with
         | "true" -> Some (Bool true)
         | "false" -> Some (Bool false)
         | w when is_int w -> Some (Int (int_of_string w))
         | w when is_var w -> Some (Var w)
         | _ -> None
       end
    | List [Atom "[]"; ty] ->
       let* ty = Type.of_sexpr_opt ty in
       Some (Nil ty)
    | List [e1; e2] ->
       let* e1 = of_sexpr_opt e1 in
       let* e2 = of_sexpr_opt e2 in
       Some (App (e1, e2))
    | List [Atom "::"; e1; e2] ->
       let* e1 = of_sexpr_opt e1 in
       let* e2 = of_sexpr_opt e2 in
       Some (Cons (e1, e2))
    | List [Atom "match"; e; arms] ->
       let* e = of_sexpr_opt e in
       let* arms = arms_of_sexpr_opt arms in
       Some (Match (e, arms))
    | List [op; e1; e2] ->
       let* op = Bop.of_sexpr_opt op in
       let* e1 = of_sexpr_opt e1 in
       let* e2 = of_sexpr_opt e2 in
       Some (Bop (op, e1, e2))
    | List [Atom "if"; e1; e2; e3] ->
       let* e1 = of_sexpr_opt e1 in
       let* e2 = of_sexpr_opt e2 in
       let* e3 = of_sexpr_opt e3 in
       Some (If (e1, e2, e3))
    | List [Atom "let"; Atom x; ty; e1; e2] ->
       let* ty = Type.of_sexpr_opt ty in
       let* e1 = of_sexpr_opt e1 in
       let* e2 = of_sexpr_opt e2 in
       Some (Let (x, ty, e1, e2))
    | List [Atom "lambda"; Atom x; ty; e] ->
       let* ty = Type.of_sexpr_opt ty in
       let* e = of_sexpr_opt e in
       Some (Fun (x, ty, e))
    | _ -> None

  let of_string_opt (s : string) : t option =
    let* se = Sexpr.of_string_opt s in
    of_sexpr_opt se
end

module Ty_error = struct
  type t =
    | Unknown_var of string
    | Wrong_type of Type.t * Type.t
    | Expected_fun of Type.t
    | Unexpected_list_pat of Type.t
    | Bound_several_times

  let to_string : t -> string = function
    | Unknown_var x -> Format.sprintf "Unbound value %s" x
    | Wrong_type (expected, actual) ->
       Format.sprintf
         "Expression has type %s but an expression was expected of type %s"
         (Type.to_string actual)
         (Type.to_string expected)
    | Expected_fun actual ->
       Format.sprintf
         "Expression has type %s. This is not a function; it cannot be applied."
         (Type.to_string actual)
    | Unexpected_list_pat actual ->
       Format.sprintf
         "Pattern matches list values but a pattern was expected which matches values of type %s"
         (Type.to_string actual)
    | Bound_several_times ->
       Format.sprintf
         "Variable is bounded several times in a match"
end

module VarMap = Map.Make (String)

module Ctxt = struct
  module M = Map.Make (String)
  type t = Type.t VarMap.t

  let empty : t = VarMap.empty
  let add : string -> Type.t -> t -> t = VarMap.add
  let singleton : string -> Type.t -> t = VarMap.singleton

  let find (x : string) (ctxt : t) : (Type.t, Ty_error.t) result =
    match VarMap.find_opt x ctxt with
    | Some ty -> Ok ty
    | None -> Error (Unknown_var x)

  let merge : t -> t -> t =
    VarMap.merge
      (fun _ a b ->
        match a, b with
        | _, Some b -> Some b
        | Some a, None -> Some a
        | _ -> None)

  let disjoint (ctxt1 : t) (ctxt2 : t) : bool =
    let merger _ a b =
      match a, b with
      | Some a, Some b -> Some ()
      | _ -> None
    in
    VarMap.merge merger ctxt1 ctxt2
    |> VarMap.is_empty
end

let ( let* ) = Result.bind
let guard (b : bool) (e : 'err) : (unit, 'err) result =
  if b then Ok () else Error e

let is_fun : Type.t -> (Type.t * Type.t, Ty_error.t) result = function
  | Type.Fun (in_ty, out_ty) -> Ok (in_ty, out_ty)
  | ty -> Error (Ty_error.Expected_fun ty)

let expected_ty (expected_ty : Type.t) (actual_ty : Type.t) : (unit, Ty_error.t) result  =
  if expected_ty = actual_ty
  then Ok ()
  else Error (Ty_error.Wrong_type (expected_ty, actual_ty))

let rec type_pat (p : Pattern.t) (ty : Type.t) : (Ctxt.t, Ty_error.t) result =
  match p, ty with
  | Wildcard, _ -> Ok Ctxt.empty
  | Int _, Int -> Ok Ctxt.empty
  | Int _, ty -> Error (Wrong_type (Int, ty))
  | Bool _, Bool -> Ok Ctxt.empty
  | Bool _, ty -> Error (Wrong_type (Bool, ty))
  | Var x, ty -> Ok (Ctxt.singleton x ty)
  | Nil, List _ -> Ok Ctxt.empty (*??*)
  | Nil, ty -> Error (Unexpected_list_pat ty)
  | Cons (p1, p2), List ty -> begin
    let* gamma1 = type_pat p1 ty in
    let* gamma2 = type_pat p2 (List ty) in
    if Ctxt.disjoint gamma1 gamma2 
      then Ok (Ctxt.merge gamma1 gamma2)
  else Error (Bound_several_times)
  end
  | Cons (_, _), ty -> Error (Unexpected_list_pat ty)

let rec type_of ctxt (e : Expr.t) : (Type.t, Ty_error.t) result =
  match e with
  | Int _ -> Ok Int
  | Bool _ -> Ok Bool
  | Var x -> Ctxt.find x ctxt
  | Nil ty -> Ok (List ty)
  | Cons (e, es) ->
     let* ty = type_of ctxt e in
     let* ty' = type_of ctxt es in
     let* _ = expected_ty (List ty) ty' in
     Ok (Type.List ty)
  | Fun (x, in_ty, e) ->
    let* out_ty = type_of (Ctxt.add x in_ty ctxt) e in 
    Ok (Type.Fun (in_ty, out_ty))
  | App (e1, e2) ->
     let* e1_ty = type_of ctxt e1 in
     let* in_ty, out_ty = is_fun e1_ty in
     let* e2_ty = type_of ctxt e2 in
     let* _ = expected_ty in_ty e2_ty in
     Ok out_ty
  | If (e1, e2, e3) ->
     let* e1_ty = type_of ctxt e1 in
     let* _ = expected_ty Bool e1_ty in
     let* e2_ty = type_of ctxt e2 in
     let* e3_ty = type_of ctxt e3 in
     let* _ = expected_ty e2_ty e3_ty in
     Ok e3_ty
     (* not sure if this was the cleanest way to do this, 
      hopefully it works*)
  | Bop (op, e1, e2) -> begin
    match (type_of ctxt e1), (type_of ctxt e2) with
    | Ok Type.Int, Ok Type.Int -> Ok Type.Int
    | _, Error y -> Error y
    | Error x, _ -> Error x
    (* this case is only hit if y is not an error, 
      the case where both are wrong is included here too*)
    | Ok x, _ -> Error (Ty_error.Wrong_type (Int, x))
    (* not sure why this case is not used... but i put it anyways
    | _, Ok y -> Error (Ty_error.Wrong_type (Int, y))*)
  end
  | Let (x, ty, e1, e2) -> begin
    let ctxt1 =
      match e1 with
      | Fun _ -> Ctxt.add x ty ctxt (* if we have a function as our e1, 
        to find its type we must pass it the type of x too *)
      | _ -> ctxt
in
    let* ty1 = type_of ctxt1 e1 in
    let* ty2 = type_of (Ctxt.add x ty ctxt) e2 in
    if (ty1 = ty) then Ok ty2
    else Error (Ty_error.Wrong_type (ty, ty1))
  end
  | Match (e0, arms) -> begin
    let* ty0 = type_of ctxt e0 in
     let type_of_arm (p, e) = 
      let* c = type_pat p ty0 in
      type_of (Ctxt.merge ctxt c) e
     in
    match arms with
    | [] -> Error (Ty_error.Wrong_type (List Int, List Int)) (* stand in error for empty arms*)
    | first :: rest ->
      let* ty = type_of_arm first in
      let rec check arms =
        match arms with
        | [] -> Ok ty (*we made it to end without errors*)
        | arm :: rest ->
          let* t = type_of_arm arm in
          if t = ty then check rest
          else Error (Ty_error.Wrong_type (ty, t))
        in
        check rest
  end

module Value = struct
  type t =
    | Int of int
    | Bool of bool
    | Nil
    | Cons of t * t
    | Clos of t VarMap.t * string * Expr.t * string option

  let rec to_string : t -> string = function
    | Int n -> string_of_int n
    | Bool b -> string_of_bool b
    | Nil -> "[]"
    | Cons (v1, v2) -> Format.sprintf "(:: %s %s)" (to_string v1) (to_string v2)
    | Clos _ -> "<fun>"
end

module Env = struct
  type t = Value.t VarMap.t

  let empty : t = VarMap.empty
  let add : string -> Value.t -> t -> t = VarMap.add
  let find : string -> t -> Value.t = VarMap.find

  let merge : t -> t -> t =
    VarMap.merge
      (fun _ a b ->
        match a, b with
        | _, Some b -> Some b
        | Some a, None -> Some a
        | _ -> None)
end

let rec matches (v : Value.t) (p : Pattern.t) : Env.t option =
  match v, p with
  | Value.Int x, Pattern.Int y when x = y-> Some Env.empty
  | Value.Bool x, Pattern.Bool y when x = y -> Some Env.empty
  | x, Pattern.Var y -> Some (Env.add y x (Env.empty))
  | Value.Nil, Pattern.Nil -> Some Env.empty
  | Value.Cons (x, a), Pattern.Cons (y, b) -> begin
    match (matches x y), (matches a b) with
    | Some env1, Some env2 -> Some (Env.merge env1 env2)
    | _ -> None
  end
  | x, Pattern.Wildcard -> Some Env.empty
  | _, _ -> None

exception Match_failure

let rec eval (env : Env.t) (e : Expr.t) : Value.t =
  let bool = function
    | Value.Bool b -> b
    | _ -> assert false
  in
  (* let int = function *)
  (*   | Value.Int n -> n *)
  (*   | _ -> assert false *)
  (* in *)
  (* let clos = function *)
  (*   | Value.Clos (env, x, e, name) -> env, x, e, name *)
  (*   | _ -> assert false *)
  (* in *)
  match e with
  | Int n -> Int n
  | Bool b -> Bool b
  | Nil _ -> Nil
  | Cons (e1, e2) -> Cons (eval env e1, eval env e2)
  | Var x -> Env.find x env
  | If (e1, e2, e3) ->
     if bool (eval env e1)
     then eval env e2
     else eval env e3
  | Fun (x, _, e) -> Clos (env, x, e, None)
  | Let (f, _, Fun (x, _, e1), e2) ->
     eval (Env.add f (Value.Clos (env, x, e1, Some f)) env) e2
  | Let (x, _, e1, e2) ->
     eval (Env.add x (eval env e1) env) e2
  | Bop (op, e1, e2) -> begin
    match eval env e1, eval env e2 with
    | Int x, Int y -> begin
    match op with
    | Add -> Int (x + y)
    | Sub -> Int (x - y)
    | Mul -> Int (x * y)
    | Eq -> Bool (x = y)
    | Neq -> Bool (x != y)
    | Lt -> Bool (x < y)
    | Lte -> Bool (x <= y)
    | Gt -> Bool (x > y)
    | Gte -> Bool (x >= y)
    end
    | _ -> assert false (* we'll never get here*)
  end
  | App (e1, e2) -> begin
    match eval env e1, eval env e2 with
    (*non rec case*)
    | (Clos (env', x, e, None)), v2 -> eval (Env.add x v2 env') e
    (*rec case*)
    | (Clos (env', x, e, Some x'), v2) -> eval (Env.add x' (Clos (env', x, e, Some x')) (Env.add x v2 env')) e
    | _ -> assert false

  end
  | Match (e, arms) ->
     let v = eval env e in
     let eval_arm (p, e) =
       match matches v p with
       | Some p_env -> Some (eval (Env.merge env p_env) e)
       | None -> None
     in
     let rec eval_arms arms =
       match arms with
       | [] -> raise Match_failure
       | arm :: arms ->
          match eval_arm arm with
          | Some v -> v
          | None -> eval_arms arms
     in eval_arms arms

let interp s =
  match Expr.of_string_opt s with
  | None -> Format.printf "Syntax error"
  | Some e ->
     match type_of Ctxt.empty e with
     | Error e -> Format.printf "Type error: %s" (Ty_error.to_string e)
     | Ok t ->
        Format.printf
          "%s : %s"
          (Value.to_string (eval Env.empty e))
          (Type.to_string t)
