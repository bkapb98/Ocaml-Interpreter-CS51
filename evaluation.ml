(*
                         CS 51 Final Project
                         MiniML -- Evaluation
                             Spring 2018
*)

(* This module implements a small untyped ML-like language under
   various operational semantics.
 *)

open Expr ;;

(* Exception for evaluator runtime, generated by a runtime error *)
exception EvalError of string ;;
(* Exception for evaluator runtime, generated by an explicit "raise" construct *)
exception EvalException ;;


(*......................................................................
  Environments and values
 *)

module type Env_type = sig
    type env
    type value =
      | Val of expr
      | Closure of (expr * env)
    val create : unit -> env
    val close : expr -> env -> value
    val lookup : env -> varid -> value
    val extend : env -> varid -> value ref -> env
    val env_to_string : env -> string
    val value_to_string : ?printenvp:bool -> value -> string
  end

module Env : Env_type =
  struct
    type env = (varid * value ref) list
     and value =
       | Val of expr
       | Closure of (expr * env)

    (* Creates an empty environment *)
    let create () : env = [] ;;

    (* Creates a closure from an expression and the environment it's
       defined in *)
    let close (exp : expr) (env : env) : value =
      Closure (exp, env) ;;

    (* Looks up the value of a variable in the environment *)
    let lookup (env : env) (varname : varid) : value =
      try
        let _, y = List.find (fun (x, _) -> x = varname) env in !y
      with
        Not_found -> raise (EvalError "var not found") ;;

    (* Returns a new environment just like env except that it maps the
       variable varid to loc *)
    let extend (env : env) (varname : varid) (loc : value ref) : env =
      let new_env = List.remove_assoc varname env in
        (varname, loc) :: new_env ;;

    (* Returns a printable string representation of a value; the flag
       printenvp determines whether to include the environment in the
       string representation when called on a closure *)
    let rec value_to_string ?(printenvp : bool = true) (v : value) : string =
      match v with
      | Val x -> "Val" ^ exp_to_concrete_string x
      | Closure (x, loc) ->
        if printenvp then
          "(" ^ exp_to_concrete_string x ^ ", " ^ env_to_string loc ^ ")"
        else value_to_string (Val x)


    (* Returns a printable string representation of an environment *)
    and env_to_string (env : env) : string =
      match env with
      | [] -> "]"
      | (x, loc) :: tl ->
        "(" ^ x ^ ", " ^ value_to_string !loc ^ env_to_string tl ;;
  end
;;


(*......................................................................
  Evaluation functions

  Returns a result of type value of evaluating the expression exp
  in the environment env. We've provided an initial implementation
  for a trivial evaluator, which just converts the expression
  unchanged to a value and returns it, along with "stub code" for
  three more evaluators: a substitution model evaluator and dynamic
  and lexical environment model versions.

  Each evaluator is of type expr -> Env.env -> Env.value for
  consistency, though some of the evaluators don't need an
  environment, and some will only return values that are "bare
  values" (that is, not closures). *)

(* The TRIVIAL EVALUATOR, which leaves the expression to be evaluated
   essentially unchanged, just converted to a value for consistency
   with the signature of the evaluators. *)

let eval_t (exp : expr) (_env : Env.env) : Env.value =
  (* coerce the expr, unchanged, into a value *)
  Env.Val exp ;;

(* The SUBSTITUTION MODEL evaluator -- to be completed *)
let eval_s (exp : expr) (_env : Env.env) : Env.value =
  let rec help (exp : expr) : expr =
  match exp with
  | Var _ -> raise (EvalError "Doesn't evaluate")
  | Num _ -> exp
  | Bool _ -> exp
  | Unop (u, e) ->
    (match u with
     | Negate -> match e with
                 | Num x -> Num (~-x)
                 | Bool x -> Bool (not x)
                 | _ -> raise (EvalError "Invalid unop type"))
  | Binop (b, e, e1) ->
    (match help e, help e1 with
     | Num y, Num z ->
       (match b with
        | Plus -> Num (y + z)
        | Minus -> Num (y - z)
        | Times -> Num (y * z)
        | Equals -> Bool (y = z)
        | LessThan -> Bool (y < z))
     | Bool y, Bool z ->
       (match b with
        | Equals -> Bool (y = z)
        | LessThan -> Bool (y < z)
        | _ -> raise (EvalError "This binop can not be used with type Bool"))
     | _ -> raise (EvalError "Binop's only take Nums or Bools"))
  | Conditional (e, e1, e2) ->
    (match help e with
     | Bool x -> if x then help e1 else help e2
     | _ -> raise (EvalError "first term of conditional must be bool"))
  | Fun (_, _) -> exp
  | Let (x, e, e1) -> help (subst x (help e) e1)
  | Letrec (x, e, e1) ->
      help (subst x (help (subst x (Letrec (x, e, (Var x))) e)) e1)
  | Raise -> raise (EvalError "raise")
  | Unassigned -> raise (EvalError "unassigned")
  | App (e, e1) ->
    (match help e with
     | Fun (x, e2) -> help (subst x (help e1) e2)
     | _ -> raise (EvalError "invalid use of app"))
  in Env.Val (help exp) ;;

(* The DYNAMICALLY-SCOPED ENVIRONMENT MODEL evaluator -- to be
   completed *)

let eval_d (_exp : expr) (_env : Env.env) : Env.value =
  failwith "eval_d not implemented" ;;

(* The LEXICALLY-SCOPED ENVIRONMENT MODEL evaluator -- optionally
   completed as (part of) your extension *)

let eval_l (_exp : expr) (_env : Env.env) : Env.value =
  failwith "eval_l not implemented" ;;

(* Connecting the evaluators to the external world. The REPL in
   miniml.ml uses a call to the single function evaluate defined
   here. Initially, evaluate is the trivial evaluator eval_t. But you
   can define it to use any of the other evaluators as you proceed to
   implement them. (We will directly unit test the four evaluators
   above, not the evaluate function, so it doesn't matter how it's set
   when you submit your solution.) *)

let evaluate = eval_s ;;
