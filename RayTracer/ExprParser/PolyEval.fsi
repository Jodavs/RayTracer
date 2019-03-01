module PolyEval

open ExprToPoly
open Types

val genRayExpr : string -> expr
val genRayPoly : string -> poly
val evalRayPoly : poly -> Ray -> Map<int, float>
val evalPoly : Map<int, float> -> float -> float
val evalPolyMap : poly -> Map<string, float> -> float -> float

val eval : Map<string, float> -> expr -> float