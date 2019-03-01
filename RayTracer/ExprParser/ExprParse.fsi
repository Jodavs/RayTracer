module ExprParse
[<Sealed>]

type terminal
exception Scanerror
val scan: char seq -> terminal list
val insertMult: terminal list -> terminal list

type expr = 
  | FNum of float
  | FVar of string
  | FAdd of expr * expr
  | FMult of expr * expr
  | FDiv of expr * expr
  | FExponent of expr * int
  | FRoot of expr * int

exception Parseerror
val parse: terminal list -> expr
val dotAST: expr -> string

val parseStr: char seq -> expr