
open Expr ;;
open Evaluation ;;
open Miniml ;;

(* expr_to_abstract_string *)
let _ =
  assert (exp_to_abstract_string (Var("x")) = "Var(x)");
  assert (exp_to_abstract_string (Binop(Times, Num 5, Var "x")) =
    "Binop(Times, Num(5), Var(x))");
  assert (exp_to_abstract_string (Binop(Divide, Num 3, Var "x" )) =
    "Binop(Divide, Num(3), Var(x))");
  assert (exp_to_abstract_string (Binop(Equals, Num 12, Var "x" )) =
    "Binop(Equals, Num(12), Var(x))");
  assert (exp_to_abstract_string (Binop(LessThan, Num 5, Var "x" )) =
    "Binop(LessThan, Num(5), Var(x))");
  assert (exp_to_abstract_string (Binop(GreaterThan, Num 5, Var("x"))) =
    "Binop(GreaterThan, Num(5), Var(x))");
  assert (exp_to_abstract_string
    (Let("f", Fun("x", Binop(Plus, Var("x"), Num 3)), App(Var("f"), Num(3)))) =
    "Let(f, Fun(x, Binop(Plus, Var(x), Num(3))), App(Var(f), Num(3)))") ;
  assert (exp_to_abstract_string (str_to_exp
    "let rec f = fun x -> if x = 0 then 1 else x * f (x - 1) in f 5;;") =
    "Letrec(f, Fun(x, Conditional(Binop(Equals, Var(x), Num(0)), Num(1), " ^
    "Binop(Times, Var(x), App(Var(f), Binop(Minus, Var(x), Num(1)))))), " ^
    "App(Var(f), Num(5)))" );
  assert (exp_to_abstract_string (Binop(Equals, Num 5, Unop(Negate, Num 5))) =
    "Binop(Equals, Num(5), Unop(Negate, Num(5)))");
  assert (exp_to_abstract_string Raise = "Raise");
  assert (exp_to_abstract_string Unassigned = "Unassigned");
  assert (exp_to_abstract_string (Bool false) = "Bool(false)");
  Printf.printf "1. exp_to_abstract_string tests passed\n" ;;

(* expr_to_concrete_string *)
let _ =
  assert (exp_to_concrete_string (Var("x")) = "x");
  assert (exp_to_concrete_string (Binop(Times, Num 5, Var "x")) =
    "(5 * x)");
  assert (exp_to_concrete_string (Binop(Divide, Num 3, Var "x" )) =
    "(3 / x)");
  assert (exp_to_concrete_string (Binop(Equals, Num 12, Var "x" )) =
    "(12 = x)");
  assert (exp_to_concrete_string (Binop(LessThan, Num 5, Var "x" )) =
    "(5 < x)");
  assert (exp_to_concrete_string (Binop(GreaterThan, Num 5, Var("x"))) =
    "(5 > x)");
  assert (exp_to_concrete_string
    (Let("f", Fun("x", Binop(Plus, Var("x"), Num 3)), App(Var("f"), Num(3)))) =
    "Let(f, Fun(x, (x + 3)), App(f, 3))") ;
  assert (exp_to_concrete_string (Binop(Equals, Num 5, Unop(Negate, Num 5))) =
    "(5 = -5)");
  assert (exp_to_concrete_string Raise = "Raise");
  assert (exp_to_concrete_string Unassigned = "Unassigned");
  assert (exp_to_concrete_string (Bool false) = "false");
  Printf.printf "2. exp_to_concrete_string tests passed\n" ;;

(* spare expressions for testing purposes *)
let a = str_to_exp "5 * x;;" ;;
let b = str_to_exp "y + x;;" ;;
let c = str_to_exp "let f = fun x -> x + 3 in f 3;;" ;;
let d = str_to_exp "if x then y else z;;" ;;
let e = str_to_exp
  "let rec f = fun x -> if x = 0 then 1 else x * f (x - 1) in f 5;;" ;;
let f = Raise ;;
let g = Unassigned ;;
let h = str_to_exp "~- 5;;" ;;
let i = str_to_exp
  "let x = 1 in let f = fun y -> x + y in let x = 2 in f 3 ;;" ;;
let i2 = str_to_exp
  "let x = 7 in let f = fun y -> x * y in let x = 5 in f 8 ;;" ;;
let j = str_to_exp "let rec f = fun x -> x * x in f 12;;" ;;
let k = str_to_exp "f 5;;" ;;
let l = str_to_exp "let rec f = fun x -> y * y in f 12;;" ;;
let m = str_to_exp "let sq = fun x -> x * x in sq 3;;" ;;
let n = str_to_exp "let x = 12 in 5 * x;;" ;;
let o = str_to_exp
  "let rec f = fun x -> if x < 21 then x * x else 144 in f 22;;";;
let p = str_to_exp
  "let rec f = fun x -> if x < 21 then x * x else 144 in f 20;;";;
let q = str_to_exp
  "let f = fun x -> if x > 3 then x / 5 else ~- x in f 7;;" ;;
let q2 = str_to_exp
  "let f = fun x -> if x < 3 then x / 5 else ~- x in f 7;;" ;;

(* free_vars *)
let _ =
  assert (free_vars a = (free_vars (Var "x")));
  assert (free_vars b = (free_vars (App(Var "y", Var "x"))));
  assert (free_vars c = (free_vars (Num 0)));
  assert (free_vars d =
    (free_vars (Binop(Minus, Var "z", Binop(Plus, Var "x", Var "y")))));
  assert (free_vars e = (free_vars (Num 0)));
  assert (free_vars f = free_vars g);
  assert (free_vars h = free_vars i);
  assert (free_vars j = free_vars (Bool(true)));
  Printf.printf "3. free_vars tests passed\n" ;;

(* subst *)
let _ =
  assert (subst "x" (Var "y") a = str_to_exp "5 * y;;");
  assert (subst "z" (Var "q") a = a);
  assert (subst "x" (Var "z") i = i);
  assert (subst "f" (Var "z") i = i);
  assert (subst "f" (Var "q") k = str_to_exp "q 5;;");
  assert (subst "y" (Var "l") d = str_to_exp "if x then l else z;;");
  assert (subst "y" (Var "q") l =
    str_to_exp "let rec f = fun x -> q * q in f 12;;");
  assert (subst "y" (Var "z") b = str_to_exp "z + x;;");
  Printf.printf "4. subst tests passed\n" ;;

(* evaluation tests *)
let z = Env.create ();;
let z1 = Env.extend z "x" (ref (Env.Val (Num 4)));;
let z2 = Env.extend z1 "y" (ref (Env.Val (Num 7)));;
let z3 = Env.extend z2 "w" (ref (Env.Val (Num (-12))));;
let z4 = Env.extend z3 "x" (ref (Env.Val (Num 13)));;

(* eval mod *)
let _ =
  assert ((Env.close a z) = (Env.Closure (a, z)));
  assert (Env.lookup z3 "x" = (Env.Val (Num 4)));
  assert (Env.lookup z4 "x" = (Env.Val (Num 13)));
  assert (Env.value_to_string (Env.Val (Num 12)) = "Val 12");
  assert (Env.env_to_string z1 = "(x, Val 4) ");
  assert (Env.env_to_string z2 = "(y, Val 7) (x, Val 4) ");
  Printf.printf "5. eval mod tests passed\n" ;;

(* Eval_s *)
(* commented out these tests since they fail when i run intentionally, but they
   correctly show that the code doesn't eval *)
let fail () =
  try eval_s a z;
  with
    Not_found -> Env.Val (Num 0) ;;
let fail2 () =
  try eval_s d z ;
  with
    Not_found -> Env.Val (Num 0) ;;

(* fail();;
fail2();; *)

let _ =
  assert (eval_s m z = Env.Val (Num(9)));
  assert (eval_s e z = Env.Val (Num 120));
  assert (eval_s i z = Env.Val (Num 4));
  assert (eval_s n z = Env.Val (Num 60));
  assert (eval_s c z = Env.Val (Num 6));
  assert (eval_s j z = Env.Val (Num 144));
  assert (eval_s o z = Env.Val (Num 144));
  assert (eval_s p z = Env.Val (Num 400));
  assert (eval_s i2 z = Env.Val (Num 56));
  assert (eval_s q z = Env.Val (Num 1));
  assert (eval_s q2 z = Env.Val (Num (~- 7)));
  Printf.printf "6. eval_s tests passed\n" ;;

(* eval_d *)
let _ =
  assert (eval_d m z = Env.Val (Num(9)));
  assert (eval_d e z = Env.Val (Num 120));
  assert (eval_d i z = Env.Val (Num 5));
  assert (eval_d n z = Env.Val (Num 60));
  assert (eval_d c z = Env.Val (Num 6));
  assert (eval_d j z = Env.Val (Num 144));
  assert (eval_d o z = Env.Val (Num 144));
  assert (eval_d p z = Env.Val (Num 400));
  assert (eval_d i2 z = Env.Val (Num 40));
  assert (eval_d q z = Env.Val (Num 1));
  assert (eval_d q2 z = Env.Val (Num (~- 7)));
  Printf.printf "7. eval_d tests passed\n" ;;

(* eval_l *)
let _ =
  assert (eval_l m z = Env.Val (Num(9)));
  assert (eval_l e z = Env.Val (Num 120));
  assert (eval_l i z = Env.Val (Num 4));
  assert (eval_l n z = Env.Val (Num 60));
  assert (eval_l c z = Env.Val (Num 6));
  assert (eval_l j z = Env.Val (Num 144));
  assert (eval_l o z = Env.Val (Num 144));
  assert (eval_l p z = Env.Val (Num 400));
  assert (eval_l i2 z = Env.Val (Num 56));
  assert (eval_l q z = Env.Val (Num 1));
  assert (eval_l q2 z = Env.Val (Num (~- 7)));
  Printf.printf "8. eval_l tests passed\n" ;;

let _ =
  Printf.printf "ALL TESTS PASSED\n";;
