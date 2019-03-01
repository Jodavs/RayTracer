module PolyEval

open Vector
open Point
open ExprParse
open ExprToPoly
open Types
open System

// Create substitution expressions for variables
let substitutes = [ ("x", parseStr "ox + t*dx"); ("y", parseStr "oy + t*dy"); ("z", parseStr "oz + t*dz") ]

let genRayExpr (s:string) =
    let e = parseStr s
    // Now substitute all x, y, z ocurrences with the expressions defined above
    List.fold (fun acc sub -> subst acc sub) e substitutes

let genRayPoly (s:string) =
    // Create the expression
    let e = parseStr s
    // Now substitute all x, y, z ocurrences with the expressions defined above
    let e' = List.fold (fun acc sub -> subst acc sub) e substitutes
    printfn "%A" e'
    // Turn the expression into a simple expr and then a polynomial solved for "t"
    (exprToSimpleExpr >> simplifyRoots >> solveRoots >> simpleExprToPoly) e' "t"

let rec evalAtom atom env =
    // Evaluates a single atom
    match atom with
    | ANum x -> x
    | AExponent(v, n) -> (Map.find v env)**float(n)
    | ARoot(num, div, n) -> (evalAtomGroups (SE num) env / evalAtomGroups (SE div) env) ** (1./(float n))
    | _ -> printfn "wtf?"; 0.
and evalAtomList ag env = List.fold (fun acc atom -> acc * (evalAtom atom env)) 1. ag // Simply shorthands for folds over evalAtom
and evalAtomGroups (SE agl) env = List.fold (fun acc ag -> acc + (evalAtomList ag env)) 0. agl

// Converts a ray to an environment. This is used by evalAtom to substitute
// variable names with their values
let rayToEnv (R(o, d)) =
    let (dx, dy, dz) = Vector.getCoord d
    let (ox, oy, oz) = Point.getCoord o
    Map.ofList [("dx",dx);("dy",dy);("dz",dz);("ox",ox);("oy",oy);("oz",oz)]

let evalRayPoly p r = 
    let env = rayToEnv r // Create env map for ray
    // Evaluate all atom groups and return the resulting map
    polyMap p |> Map.map (fun k v -> evalAtomGroups v env)

let evalPoly pMap x =
    Map.fold (fun acc k v -> acc + x**(float k)*v) 0. pMap
  
let evalPolyMap poly env x =
    let mapn = polyMap poly |> Map.map (fun k v -> evalAtomGroups v env)
    evalPoly mapn x

/// <summary>
/// Checks if the solution to a solved root is correct
/// </summary>
/// <param name="original">Original simpleExpr</param>
/// <param name="x">x solution</param>
/// <param name="y">y solution</param>
/// <param name="z">z solution</param>
let checkSolution original x y z =
    let env = Map.ofList [("x", x); ("y", y); ("z", z)]
    evalAtomGroups original env = 0.

let rec eval env = function
  | FNum c -> c
  | FVar s -> match Map.tryFind s env with
                Some v -> v
              | None -> failwith "no matching variable in env"
  | FAdd(e1,e2) -> (eval env e1) + (eval env e2)
  | FMult(e1,e2) -> (eval env e1) * (eval env e2)
  | FDiv(e1, e2) -> (eval env e1) / (eval env e2)
  | FExponent(e1,0) -> 1.
  | FExponent(e1,1) -> eval env e1
  | FExponent(e1,n) -> (eval env e1)**(float n)
  | FRoot(e, n)     -> (eval env e)**(1./(float n))