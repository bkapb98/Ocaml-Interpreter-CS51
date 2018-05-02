type token =
  | EOF
  | OPEN
  | CLOSE
  | LET
  | DOT
  | IN
  | REC
  | NEG
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | LESSTHAN
  | EQUALS
  | GREATERTHAN
  | IF
  | THEN
  | ELSE
  | FUNCTION
  | RAISE
  | ID of (string)
  | INT of (int)
  | TRUE
  | FALSE

val input :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Expr.expr
