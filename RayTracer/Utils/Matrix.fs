module Matrix

open Types
open Point
open Vector

type Matrix = M of float array array

type Matrix with
    static member ( * ) (m1:Matrix, m2) = m1.MultMatrix m2
    static member identity = M([|[|1.;0.;0.;0.|];
                                 [|0.;1.;0.;0.|];
                                 [|0.;0.;1.;0.|];
                                 [|0.;0.;0.;1.|]|]);

    member m.GetArray = 
        let (M(ma)) = m
        ma

    member m.Get x y = m.GetArray.[x].[y]
    member m.Set x y v = m.GetArray.[x].[y] <- v
    member m.SetColumn (i, col) = m.GetArray.[i] <- col
    member m.SetColumn (i, v1, v2, v3, v4) = m.GetArray.[i] <- [| v1; v2; v3; v4; |]

    member m.GetRow i = [| m.Get 0 i; m.Get 1 i; m.Get 2 i; m.Get 3 i; |]

    member m.Transpose =
        let outMatrix = Matrix.identity
        outMatrix.SetColumn (0, m.GetRow 0)
        outMatrix.SetColumn (1, m.GetRow 1)
        outMatrix.SetColumn (2, m.GetRow 2)
        outMatrix.SetColumn (3, m.GetRow 3)
        outMatrix

    member m.MultScalar s = Array.map (fun col -> Array.map ((*)s) col) m.GetArray |> M
    
    member m.MultRay ray = 
        let (R(o, d)) = ray
        let oArray = [| Point.getX o;  Point.getY o;  Point.getZ o;  1. |]
        let dArray = [| Vector.getX d; Vector.getY d; Vector.getZ d; 0. |]
        let (newOArray:float array), newDArray = m.MultVectorArray oArray, m.MultVectorArray dArray

        let last = newOArray.[3]
        let newO = mkPoint  (newOArray.[0]/last) (newOArray.[1]/last) (newOArray.[2]/last)
        let newD = mkVector (newDArray.[0])   (newDArray.[1])   (newDArray.[2])
        R(newO, newD)

    member m.MultVector vec = 
        let vecArray = [| Vector.getX vec; Vector.getY vec; Vector.getZ vec; 0. |]
        let resArray = m.MultVectorArray vecArray
        Vector.mkVector resArray.[0] resArray.[1] resArray.[2]
    
    member m.MultPoint point = 
        let vecArray = [| Point.getX point; Point.getY point; Point.getZ point; 1. |]
        let resArray = m.MultVectorArray vecArray
        Point.mkPoint resArray.[0] resArray.[1] resArray.[2]

    member m.MultVectorArray vec =
        let (M(tm)) = m.Transpose
        let rowMult row = Array.fold2 (fun acc me pe -> acc + me*pe) 0. row vec
        Array.map rowMult tm

    member m.MultMatrix (M(m2)) =
        let mt = Matrix.identity
        for i in [0..3] do
            mt.SetColumn(i, m.MultVectorArray m2.[i])
        mt

