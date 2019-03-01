module ManyDegree
 
open ExprToPoly
open PolyEval
open System

let roundDigits = 10
 
 // Derive a polynomial
let derive (P(poly)) =
      // Remove 0'th element since constants are remove when derived
      let mapWithoutZeroElement = Map.remove 0 poly
      // Run through n'th Expr, move the 5'th to the 4'th and multiply (concatenate) the constant with the Expr
      let newMap = Map.foldBack (fun k (SE(v)) (acc: Map<int, simpleExpr>) ->
                   acc.Add (k-1, SE(List.map (fun x -> (ANum (float k))::x) v))
                   ) mapWithoutZeroElement Map.empty
      P(newMap)


(*
 * This is the pseudo code from wikipedia: https://en.wikipedia.org/wiki/Polynomial_long_division
function n / d:
  require d ≠ 0
  q ← 0
  r ← n       # At each step n = d × q + r
  while r ≠ 0 AND degree(r) ≥ degree(d):
     t ← lead(r)/lead(d)     # Divide the leading terms
     q ← q + t
     r ← r − t * d
  return (q, r)
*)

// takes a term and a simple polynomial: (int*float) list
let multiplyPoly (term:int*float) =
    // The power "ti" and the value "tf"
    let ti, tf = term
    // Add the powers and multiply the values
    Map.fold (fun acc i (f:float) -> if tf*f <> 0. then Map.add (ti+i) (tf*f) acc else acc) Map.empty

// takes to simple polynomials and subtracts them
let subtractPoly m1 m2 =
    // Run through map m1 and subtracts the values from m2
    let a = Map.fold (fun (acc:Map<int, float>) i f -> 
        match Map.tryFind i m2 with
        | None -> acc.Add(i, f)
        | Some (f2:float) -> acc.Add(i, Math.Round(f, roundDigits) - Math.Round(f2, roundDigits) )) Map.empty m1
    // Run through m2 and add the values that weren't found in m1
    Map.fold (fun (acc:Map<int, float>) i f ->
        match Map.tryFind i a with
        | None -> acc.Add(i, -f)
        | Some _ -> acc ) a m2

// expressions must be ordered from largest to smallest degree term          
let polyLongDivision (dividend:Map<int, float>) (divisor:Map<int, float>) = 
    // dividend: the number to be divided, divisor: the number to divide by
    let mutable q = 0.

    // Initialize the remainder
    let mutable r = dividend

    // Check if all the values are 0 by adding the values together
    let isNotZero l = 0. <> Map.fold (fun acc _ v -> acc+v) 0. l

    // Get the highest int,float pair, which value is 0.
    let first l = 
        Map.fold (fun (maxDegree, value) k v -> if k > maxDegree && v <> 0. 
                                                then (k, v) 
                                                else (maxDegree, value)) (0, 0.) l
    // Same as first, but takes the int that represents the power
    let degree l = first l |> fst

    // Continue running the division until the remainder is 0 or the degree of the 
    // remainder is smaller than the degree of the divisor
    while isNotZero r && degree(r) >= degree(divisor) do
        let (i1, f1) = first r
        let (i2, f2) = first divisor
        // f2 should not be 0 because it's the divisor
        if f2 <> 0. then
            // Division
            let t = (i1-i2, Math.Round(f1, roundDigits)/Math.Round(f2, roundDigits))
            //Calculate r according to the formula in lecture notes
            r <- multiplyPoly t divisor |> subtractPoly r 
    r


 
/// <summary>
/// Calculates roots using Newton Raphson's method. To use, first evaluate a polynomial and its derivative using evalRayPoly
/// </summary>
/// <param name="polyMap">Contains each term of a polynomial (constant and power)</param>
/// <param name="derivedMap">Contains each term of the derived polynomial</param>
/// <param name="a">The value to start the method at</param>
/// <returns>-1 if the method did not converge on a root and otherwise a float value signifying the value of the found root</returns>
let raphson polyMap derivedMap a =
    // This contains just the formula for calculating the next iteration in Newton Raphon's method
    let raphsonHelper a = a - (evalPoly polyMap a / (evalPoly derivedMap a))
    // This is what actually calculates each iteration of the method
    let rec iterate acc diff =
        match diff with
        // If the difference is smaller than this number, the method converged and we return the result
        | diff when diff < 1.0E-4 -> if acc > 0. then Some(acc) else None
        // Otherwise calculate the next iteration
        | diff -> let next = raphsonHelper acc // This is the value of the next iteration
                  let nextDiff = abs(next-acc) // Now calculate the new difference between iterations
                  // If this difference is larger than the last one, the method is diverging
                  if nextDiff > diff then None 
                  else iterate next nextDiff // Otherwise we calculate the next iteration
    // Start the method from the chosen value a. This is also used as the starting difference
    iterate a System.Double.MaxValue


let genSturmSeq poly ray = 
    // find f'(x)*x
    // run polyLongDivision until we only have a constant
    let f' = derive poly
    // This fold is equivalent to multiplying f'(x) by x, hence adding 1 to the power

    // First poly
    let p0 = evalRayPoly poly ray
    
    // Second poly
    let p1 = evalRayPoly f' ray

    // Get the highest int,float pair, which value is 0.
    let first l = 
        Map.fold (fun (maxDegree, value) k v -> if k > maxDegree && v <> 0. 
                                                then (k, v) 
                                                else (maxDegree, value)) (0, 0.) l
    
    // Same as first, but takes the int that represents the power
    let degree l = first l |> fst

    // Generate the sequence
    let rec genSeq p p' acc =
        // We're done
        if degree p' = 0 then acc
        elif degree p <= 1 then acc
        else
            // Do the long division and multiply it with -1
            let pNext = polyLongDivision p p' |> multiplyPoly (0, -1.)
            genSeq p' pNext (pNext::acc)
    
    // Call the genSeq with 2 first polys hardcored into
    let s = genSeq p0 p1 [p1; p0]
    s

// Returns the amount of roots in a given range
let numberOfRoots a b sturmSeq =
    // Calculate the polys 
    let calcRes x = List.map (fun poly -> evalPoly poly x) sturmSeq
    // Calculate all polys for a and b
    let aRes = calcRes a
    let bRes = calcRes b
    // Calculate sign changes
    let calcSignChanges = List.fold (fun (last, acc) v -> 
                                        match v with
                                        // If the value is 0, skip it
                                        | 0. -> (last, acc)
                                        // If the 2 numbers are < 0, the sign has changed since both + * + and - * - give positiv numbers
                                        | _ when last*v < 0. -> (v, acc+1)
                                        // return the accumulator
                                        | _ -> (v, acc)) (0., 0) >> snd
    // Calculate the sign changes for both a and b
    let aSignChanges = calcSignChanges aRes
    let bSignChanges = calcSignChanges bRes   
    // Return the difference, which represents the amount of roots
    aSignChanges-bSignChanges

// Get the smallest positive root
let getMinRoot poly derived ray maxDist = 
    // Evaluate the poly
    let polyMap = evalRayPoly poly ray
    // Evaluate the derived poly
    let derivedMap = evalRayPoly derived ray

    let sturm = genSturmSeq poly ray
        
    let bisection a b tolerance maxIterations =
        let rec bisectIter a b nIterations =
            let newMid = (a+b)/2.
            let numRoots = numberOfRoots a newMid sturm
            let polyValue = evalPoly polyMap newMid
            if nIterations > maxIterations then failwith "no solution found"
            if polyValue = 0. || (b-a)/2. < tolerance then newMid
            elif numRoots = 0 then bisectIter newMid b (nIterations+1)
            else bisectIter a newMid (nIterations+1)
        bisectIter a b 0
    if numberOfRoots 0. maxDist sturm = 0 then None
    else

        let bestGuess = bisection 0. maxDist 0.01 10000

        match raphson polyMap derivedMap bestGuess with
          Some x -> Some x
        | None -> 
            let betterGuess = bisection 0. maxDist 0.001 100000
            raphson polyMap derivedMap betterGuess