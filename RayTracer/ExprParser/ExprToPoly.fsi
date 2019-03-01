module ExprToPoly

type expr = ExprParse.expr
val subst: expr -> (string * expr) -> expr

type atom = ANum of float | AExponent of string * int | ARoot of (atom list list) * (atom list list) * int
type atomGroup = atom list  
type simpleExpr = SE of atomGroup list
val ppSimpleExpr: simpleExpr -> string
val exprToSimpleExpr: expr -> simpleExpr

val simplifyRoots: simpleExpr -> simpleExpr
val solveRoots: simpleExpr -> simpleExpr

type poly = P of Map<int,simpleExpr>
val polyMap: poly -> Map<int,simpleExpr>
val ppPoly: string -> poly -> string
val simpleExprToPoly: simpleExpr -> string -> poly
val stringToPoly: string -> (string -> poly)
