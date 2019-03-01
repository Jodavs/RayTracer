module ManyDegree
 
open ExprToPoly
open PolyEval
open System

// First of all: Polynomials are represented by a Map<int, float>, the int indicating the power and float the value.

/// Derive a polynomial, example: x^2 -> 2x
val derive : poly -> poly

/// Multiply polynomials, which is required in polyLongDivision.
val multiplyPoly : int * float -> (Map<int, float> -> Map<int, float>)

/// Subtract polynomials, which is required in polyLongDivision.
val subtractPoly : Map<int, float> -> Map<int, float> -> Map<int, float>

/// Poly long division is used to generate the sturm sequence, by recursively long dividing polynomials
val polyLongDivision : Map<int, float> -> Map<int, float> -> Map<int, float>

/// Calculates roots using Newton Raphson's method. It's done by recursively converging on the root.
val raphson : Map<int, float> -> Map<int, float> -> float -> float option

/// Generates a sturm sequence, which is a list of polynomials. 
/// It is used in the method numberOfRoots, which calculates the number of roots in a given range
val genSturmSeq : poly -> Types.Ray -> Map<int, float> list

/// Calculates the number of roots in a range between a and b. 
/// It's done by subtracting the signchanges from the calculated values in the sturm sequence using a and b, respectively.
val numberOfRoots : float -> float -> Map<int, float> list -> int

/// Ties everything together, this is the function called from the outside.
/// Get the smallest positive root.
val getMinRoot : poly -> poly -> Types.Ray -> float -> float option