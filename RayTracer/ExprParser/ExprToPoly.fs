module ExprToPoly

(*#load "ExprParse.fs"*)

open ExprParse
type expr = ExprParse.expr

let rec ppExpr = function
  | FNum c -> string(c)
  | FVar s -> s
  | FAdd(e1,e2) -> "(" + (ppExpr e1) + " + " + (ppExpr e2) + ")"
  | FMult(e1,e2) -> (ppExpr e1) + " * " + (ppExpr e2)
  | FExponent(e,n) -> "(" + (ppExpr e) + ")^" + string(n)
  | FDiv(e1, e2) -> "(" + (ppExpr e1) + ")/(" + (ppExpr e2) + ")"
  | FRoot(e, n) -> "√" + (string n) + "(" + (ppExpr e) + ")"

let rec subst e (x,ex) =
  match e with   
  | FNum c -> FNum c 
  | FVar s -> if s = x then ex else e
  | FMult(e1,e2) -> FMult(subst e1 (x,ex), subst e2 (x,ex))
  | FAdd(e1,e2)  -> FAdd(subst e1 (x,ex), subst e2 (x,ex))
  | FExponent(e, n) -> FExponent(subst e (x,ex), n)
  | FDiv(e1, e2) -> FDiv(subst e1 (x,ex), subst e2 (x,ex))
  | FRoot(e, n) -> FRoot(subst e (x,ex), n)

// Root atom is not really an atom because it cannot immediatly be reduced 
type atom = ANum of float | AExponent of string * int | ARoot of (atom list list) * (atom list list) * int
type atomGroup = atom list  
type simpleExpr = SE of atomGroup list
let isSimpleExprEmpty (SE ags) = ags = [] || ags = [[]]

let ppAtom = function
  | ANum c -> string(c)
  | AExponent(s,1) -> s
  | AExponent(s,n) -> s+"^"+(string(n))
let ppAtomGroup ag = String.concat "*" (List.map ppAtom ag)
let ppSimpleExpr (SE ags) = String.concat "+" (List.map ppAtomGroup ags)

let rec combine xss = function
  | [] -> []
  | ys::yss -> List.map ((@) ys) xss @ combine xss yss

/// <summary>
/// takes an expression and puts it to the nth power. (e)^n
/// </summary>
/// <param name="original">expression to be combined</param>
/// <param name="n">nth power</param>
let combineN original n =
    let rec combineHelper (original:atom list list) (acc: atom list list) = function // Basically original^n
        | 1 -> acc
        | n -> combineHelper original (combine original acc) (n-1)
    combineHelper original original n


let rec simplify = function
  | FNum c -> ([[ANum c]], [[ANum 1.]])
  | FVar s -> ([[AExponent(s,1)]], [[ANum 1.]])
  | FAdd(e1,e2) -> let num1, div1 = simplify e1 // simplify each part of addition
                   let num2, div2 = simplify e2
                   let numres1 = combine num1 div2 // add fractions together a/b+c/d = (ad+bc)/bd
                   let numres2 = combine num2 div1
                   let divres = combine div1 div2
                   (numres1 @ numres2, divres)
  | FMult(e1,e2) -> let num1, div1 = simplify e1 // simplify each part of multiplication
                    let num2, div2 = simplify e2
                    (combine num1 num2, combine div1 div2) // multiply fractions a/b*c/d = ab/cd
  | FDiv(e1, e2) -> let num1, div1 = simplify e1 // simplify each part of division
                    let num2, div2 = simplify e2
                    let numres = combine num1 div2 // divide fractions (a/b)/(c/d) = ad/bc
                    let divres = combine div1 num2
                    (numres, divres)
  | FExponent(e1,0) -> ([[ANum 1.]], [[ANum 1.]])
  | FExponent(e1,1) -> simplify e1
  | FExponent(e1,n) -> let num, div = simplify e1
                       let numr, divr = FExponent(e1, n-1) |> simplify 
                       (combine num numr, combine div divr) // multiply fractions together
  | FRoot(e, n)     -> let num, div = simplify e
                       ([[ARoot(num, div, n)]], [[ANum 1.]]) // create root atom
 

let simplifyAtomGroup ag = 
  let mapCount = fun ((m:Map<string,int>), c) e -> 
    match e with
    | AExponent(v, n) ->
      match Map.tryFind v m with
      | Some x -> (Map.add v (x+n) m, c)
      | None   -> (Map.add v n m, c)
    | ANum c' -> (m, c*c')
    | _ -> (m, c)

  let roots = List.filter (fun e -> match e with
                                    | ARoot _ -> true
                                    | _ -> false) ag


  let (collectedTerms, c) = List.fold mapCount (Map.empty, 1.0) ag
  let terms = roots @ List.map AExponent (Map.toList collectedTerms)

  match c with
    | 0.0 -> []
    | 1.0 -> terms
    | _ -> ANum c :: terms                    


let simplifySimpleExpr (SE ags) =
  let ags' = List.map simplifyAtomGroup ags
  // Add atom groups with only constants together.
  let collectConst = fun (acc, (li:atom list list)) e ->
    match e with
    | [ANum c] -> (acc+c, li)
    | _        -> (acc, e::li)
  let (total, agsV) = List.fold collectConst (0.0, []) ags'

  // Last task is to group similar atomGroups into one group.
  let groupCount = fun (m:Map<atomGroup,int>) e -> 
    match Map.tryFind e m with
    | Some x -> Map.add e (x+1) m
    | None   -> Map.add e 1 m
     
  let agsG = List.fold groupCount Map.empty agsV

  let foldToList = fun li k v ->
    match v with
    | 1 -> k::li
    | _ -> (ANum (float(v)) :: k) :: li
     
  let agsL = List.rev <| Map.fold foldToList [] agsG // I have to reverse the list

  SE( if total <> 0.0
      then [ANum total] :: agsL
      else agsL )


/// <summary>
/// Simplify roots that are set to a power greater than 1
/// </summary>
let simplifyRoots (SE exp) =
    // Helper function for counting number of roots in a atom list
    let rootCount = fun (r:Map<((atom list list)*(atom list list)*int),int>) e -> 
        match e with
        | ARoot(num, div, n) -> // If a root is found, increase the counter in the map by one. Otherwise add it to the map
            match Map.tryFind (num, div, n) r with
            | Some x -> Map.add (num, div, n) (x+1) r
            | None   -> Map.add (num, div, n) 1 r
        | _ -> r

    // Count total number of roots 
    let atomGroupRoots = List.fold rootCount (Map.empty)
    // Count for each atom group
    let rootMaps = List.map atomGroupRoots exp

    
    let genRootTerm e = // What it actually does is simplify the roots for a single term
        let removeTermsFromRoot (root:atom list) = List.partition (fun e -> match e with
                                                                        | ARoot _ -> false
                                                                        | _ -> true) root
        let terms, roots = removeTermsFromRoot e // Split term into non-root part and root-part

        let rootMap =  List.fold rootCount (Map.empty) roots // Create a rootmap (which counts the occurences of each root)

        // Depending on the exponent of the root, either just return the root, or remove the root.
        // The exponent has to be a multiple of the root degree, since we will otherwise just get a new root.
        // Example: sqrt(2)^4 = 2^2
        let extractRoot ((num, div, n):((atom list list)*(atom list list)*int)) count =
            let root = (num, div, n)
            match count with
            | a when a < n -> ([[ARoot(num, div, n)]], [[ANum 1.]]) // Because the exponent is too low, we just return the original root
            | a when a % n = 0 -> let exp = a/n // Get exponent of part of expression without root
                                  (combineN num exp, combineN div exp) // Calculate the fraction of the root exponentiated to the (a/n)th power
            | _ -> failwith "root exponent didn't match" // Panic if they don't match

        // Extract root and multiply with fraction - (num, div) is the accumulator for the fraction expression so far.
        let multiplyRoot (num, div) root count = 
            let (rootnum, rootdiv) = extractRoot root count
            (combine rootnum num, combine rootdiv div) // Multiply the extracted root onto the accumulator

        // This is where everything begins.
        // Take every root in the current term and map the multiplyRoot function over it.
        // The result is effectively that every exponentiated root is simplified
        Map.fold multiplyRoot ([terms], [[ANum 1.]]) rootMap

    // Simplify the roots of every term in the expression
    let simplifiedTerms = List.map genRootTerm exp
    // Combine the terms so we get a new term which is a single fraction
    // (which makes it possible to discard the denominator)
    let combinedTerms = 
        List.reduce (fun (resnum, resdiv) (num, div) -> 
                            (combine resnum div @ combine num resdiv, combine resdiv div) // Add current fraction to accumulator
                    ) simplifiedTerms
    SE(fst combinedTerms) // Only return the numerator

let exprToSimpleExpr e = simplifySimpleExpr (SE (simplify e |> fst))

let exprToSimpleDivExpr e = let (num, div) = simplify e
                            (simplifySimpleExpr (SE num), simplifySimpleExpr (SE div))

/// Simplifies roots of simpleExpr by repeatedly isolating a root on one side of the expression and exponentiating both sides.
let solveRoots (SE exp) =
    // Returns a bool indicating whether there is a root in the group or not
    let isRootGroup (group:atom list) = List.exists (fun e -> match e with
                                                              | ARoot _ -> true
                                                              | _ -> false) group

    let partitionTermIntoRootAndRest (root:atom list) = List.partition (fun e -> match e with
                                                                        | ARoot _ -> false
                                                                        | _ -> true) root
    // This is the actual method doing the simplification
    let rec simplify (before: atom list list) = function
    | x::xs when isRootGroup x -> // Only simplify atomGroups containing roots
        let complete = before @ xs // Creates an exp that contains the whole expression except the current root term
        let rootTerms, root = partitionTermIntoRootAndRest x // Split root term into root and rest
        match root with
        | ARoot(rnum, rdiv, n) :: _ -> 
            // Move the expression "complete" to other side of the equal sign by multiplying by -1
            let cominv = List.map (fun e -> if e <> [] then ANum -1. :: e else e) complete
            // Now both the root needs to be isolated. Because there might be more terms in the root group
            // than the root itself, we need to divide by it on both sides on the equal sign.
            // This is done here by simply making complete the numerator and the rootterms the denominator of a fraction.
            // Example: sqrt(2)*4x = 5y <-> sqrt(2) = 5y/4x
            let cnum, cdiv = cominv, [rootTerms] // Divide cominv by the rootterms
            // Now we remove the root by exponentiating both sides of the equation by the degree of the root
            let newcnum, newcdiv = combineN cnum n, combineN cdiv n // put expression to the power of the root
            // Now we need to move the (complete/rootterms) expression back to the other side of the equation.
            // This is done by multiplying by -1 for both the numerator and denominator.
            let newcnuminv = List.map (fun e -> ANum -1. :: e) newcnum
            let newcdivinv = List.map (fun e -> ANum -1. :: e) newcdiv
            // Now we add the two expressions together.
            // We also call the simplifyRoots function to check if any new nested
            // roots can be simplified.
            // Finally we simplify the expression because a lot of extra atoms have emerged from the repeated combinations
            let (SE fcnum) = (combine newcnuminv rdiv) @ (combine rnum newcdiv) |> SE |> simplifyRoots |> simplifySimpleExpr
            let (SE fcdiv) = combine rdiv newcdiv |> SE |> simplifyRoots |> simplifySimpleExpr
            // Because we have encountered a root, we need to start from the beginning. This is done in order to
            // cover a case when a root contains another nested root.
            // Note: this method doesn't work if a term contains roots with different degrees (could be solved by only using the first root but is not required by any of the tested formulas)
            simplify [] fcnum
        | _ -> (x::xs)  
    | x::xs -> simplify (x::before) xs // Simply skip groups without roots
    | _ -> before // We are done and return the simplified expression

    // Run the simplify function and simplify the result (it contains a lot of 1's)
    simplifySimpleExpr (SE(simplify [] exp)) 

type poly = P of Map<int,simpleExpr>

let ppPoly v (P p) =
  let pp (d,ags) =
    let prefix = if d=0 then "" else ppAtom (AExponent(v,d))
    let postfix = if isSimpleExprEmpty ags then "" else "(" + (ppSimpleExpr ags) + ")"
    prefix + postfix
  String.concat "+" (List.map pp (Map.toList p))

(* Collect atom groups into groups with respect to one variable v *)
let splitAG v m = function
  | [] -> m
  | ag ->
    let eqV = function AExponent(v',_) -> v = v' | _ -> false
    let addMap d ag m = 
      match Map.tryFind d m with
      | Some (SE x) -> Map.add d (SE (ag::x) ) m
      | None   -> Map.add d (SE [ag]) m
    match List.tryFind eqV ag with
      | Some (AExponent(_,d)) ->
        let ag' = List.filter (not << eqV) ag
        addMap d ag' m
      | Some _ -> failwith "splitAG: Must never come here! - ANum will not match eqV"
      | None -> addMap 0 ag m

let simpleExprToPoly (SE ags) v =
  P (List.fold (splitAG v) Map.empty ags)

let exprToPoly e v = (exprToSimpleExpr >> simplifySimpleExpr >> simplifyRoots >> solveRoots >> simpleExprToPoly) e v

let stringToPoly (s:string) = exprToPoly (parseStr s)

let polyMap (P m) = m