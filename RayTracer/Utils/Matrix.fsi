module Matrix

open Types
open Point
open Vector

type Matrix = M of float array array

type Matrix with
    static member ( * ) : Matrix * Matrix -> Matrix
    static member identity : Matrix

    member GetArray : float array array

    member Get : x:int -> y:int -> float
    member Set : x:int -> y:int -> value:float -> unit
    member SetColumn : int * float array -> unit
    member SetColumn : int * float * float * float * float -> unit

    member GetRow : int -> float array

    member Transpose : Matrix

    member MultScalar : float -> Matrix
    member MultRay : Ray -> Ray
    member MultVector : Vector -> Vector
    member MultPoint : Point -> Point
    member MultVectorArray : float array -> float array
    member MultMatrix : Matrix -> Matrix