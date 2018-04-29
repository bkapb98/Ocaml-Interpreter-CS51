(*
                         CS 51 Final Project
                        MiniML -- Expressions
                             Spring 2018
*)

(*......................................................................
  Abstract syntax of MiniML expressions
 *)

type unop =
  | Negate
;;

type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
;;

type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
 and varid = string ;;

(*......................................................................
  Manipulation of variable names (varids)
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars :  varidset -> varidset -> bool
   Test to see if two sets of variables have the same elements (for
   testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list : string list -> varidset
   Generate a set of variable names from a list of strings (for
   testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;

(* free_vars : expr -> varidset
   Return a set of the variable names that are free in expression
   exp *)
let rec free_vars (exp : expr) : varidset =
  match exp with
  | Var x -> SS.singleton x
  | Num _ -> SS.empty
  | Bool _ -> SS.empty
  | Unop (_, e) -> free_vars e
  | Binop (_, e, e1) -> SS.union (free_vars e) (free_vars e1)
  | Conditional (e, e1, e2) ->
    SS.union (SS.union (free_vars e) (free_vars e1)) (free_vars e2)
  | Fun (x, e) -> SS.remove x (free_vars e)
  | Let (x, e, e1) -> SS.union (SS.remove x (free_vars e1)) (free_vars e)
  | Letrec (x, e, e1) ->
    SS.union (SS.remove x (free_vars e)) (SS.remove x (free_vars e1))
  | Raise -> SS.empty
  | Unassigned -> SS.empty
  | App (e, e1) -> SS.union (free_vars e) (free_vars e1) ;;


(* new_varname : unit -> varid
   Return a fresh variable, constructed with a running counter a la
   gensym. Assumes no variable names use the prefix "var". (Otherwise,
   they might accidentally be the same as a generated variable name.) *)
(* taken from lab5  *)
let new_varname () : varid =
  let gensym =
  let ctr = ref 0 in
  fun (s : string) ->
    let v = s ^ string_of_int (!ctr) in
    ctr := !ctr + 1;
    v in gensym "y" ;;

(*......................................................................
  Substitution

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst : varid -> expr -> expr -> expr
   Substitute repl for free occurrences of var_name in exp *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  match exp with
  | Var x -> if var_name = x then repl else exp
  | Num x -> exp
  | Bool x -> exp
  | Unop (u, e) -> Unop(u, subst var_name repl e)
  | Binop (b, e, e1) ->
    Binop(b, subst var_name repl e, subst var_name repl e1)
  | Conditional (e, e1, e2) -> Conditional (subst var_name repl e,
    subst var_name repl e1, subst var_name repl e2)
  | Fun (x, e) ->
    if var_name = x then exp
    else if SS.mem x (free_vars repl) then (subst (new_varname()) repl exp)
    else Fun (x, subst var_name repl e)
  | Let (x, e, e1) ->
    if var_name = x then exp
    else if SS.mem x (free_vars repl) then
      let a = new_varname() in
      Let (a, e, subst var_name repl (subst x (Var a) e1))
    else Let (x, subst var_name repl e, subst var_name repl e1)
  | Letrec (x, e, e1) ->
    if var_name = x then exp
    else if SS.mem x (free_vars repl) then
      let a = new_varname() in
      Let (a, subst var_name repl (subst x (Var a) e),
        subst var_name repl (subst x (Var a) e1))
    else Letrec (x, subst var_name repl e, subst var_name repl e1)
  | Raise -> Raise
  | Unassigned -> Unassigned
  | App (e, e1) -> App(subst var_name repl e, subst var_name repl e1);;

(*......................................................................
  String representations of expressions
 *)


(* exp_to_concrete_string : expr -> string
   Returns a concrete syntax string representation of the expr *)
(* used a helper function with a shorter name to make the code easier to read *)
let exp_to_concrete_string (exp : expr) : string =
  let rec help (exp : expr) : string =
  match exp with
  | Var x -> "x"
  | Num x -> string_of_int x
  | Bool x -> string_of_bool x
  | Unop (u, e) ->
    (match u with
     | Negate -> "-") ^ help e
  | Binop (b, e, e1) -> "(" ^ help e ^ ")" ^
    (match b with
     | Plus -> "+"
     | Minus -> "-"
     | Times -> "*"
     | Equals -> "="
     | LessThan -> ">")  ^ "(" ^ help e1 ^ ")"
  | Conditional (e, e1, e2) ->
    "(if" ^ "(" ^ help e ^ ")" ^ "then" ^ "(" ^ help e1 ^ ")"  ^
     "else" ^ "(" ^ help e2 ^ ")"  ^ ")"
  | Fun (x, e) -> "Fun(" ^ x ^ ", " ^ help e ^ ")"
  | Let (x, e, e1) -> "Let(" ^ x  ^ ", " ^ help e ^ "," ^
    help e ^  ")"
  | Letrec (x, e, e1) -> "Letrec(" ^ x  ^ ", " ^ help e ^
    ", " ^ help e ^  ")"
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App (e, e1) -> "App(" ^ help e ^ ", " ^
    help e ^  ")" in help exp ;;

(* exp_to_abstract_string : expr -> string
   Returns a string representation of the abstract syntax of the expr *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  | Var x -> "Var(" ^ x ^ ")"
  | Num x -> "Num(" ^ string_of_int x ^ ")"
  | Bool x -> "Bool(" ^ string_of_bool x ^ ")"
  | Unop (u, e) -> "Unop(" ^
    (match u with
     | Negate -> "Negate") ^ ", " ^ exp_to_abstract_string e  ^ ")"
  | Binop (b, e, e1) -> "Binop(" ^
    (match b with
     | Plus -> "Plus"
     | Minus -> "Minus"
     | Times -> "Times"
     | Equals -> "Equals"
     | LessThan -> "LessThan") ^ ", " ^ exp_to_abstract_string e  ^ ", "
        ^ exp_to_abstract_string e1 ^ ")"
  | Conditional (e, e1, e2) -> "Conditional(" ^ exp_to_abstract_string e  ^
    "," ^ exp_to_abstract_string e1  ^ ", " ^ exp_to_abstract_string e2 ^ ")"
  | Fun (x, e) -> "Fun(" ^ x ^ ", " ^ exp_to_abstract_string e ^ ")"
  | Let (x, e, e1) -> "Let(" ^ x  ^ ", " ^ exp_to_abstract_string e ^ "," ^
    exp_to_abstract_string e1 ^  ")"
  | Letrec (x, e, e1) -> "Letrec(" ^ x  ^ ", " ^ exp_to_abstract_string e ^
    ", " ^ exp_to_abstract_string e1 ^  ")"
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App (e, e1) -> "App(" ^ exp_to_abstract_string e ^ ", " ^
    exp_to_abstract_string e1 ^  ")" ;;

