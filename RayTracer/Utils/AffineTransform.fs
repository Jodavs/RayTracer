module AffineTransform

open Vector
open Point
open Types
open Matrix

type AffineTransform (m1 : Matrix, m2 : Matrix, idMatrix : bool) =
    member t.Get = m1
    member t.GetInverse = m2
    member t.IsIdentity = idMatrix

let identity = new AffineTransform(Matrix.identity, Matrix.identity, true)


let compose (transforms : AffineTransform seq) : AffineTransform =
    let matrix    = Seq.map (fun (t:AffineTransform) -> t.Get) transforms |> Seq.reduce (fun acc trans -> trans * acc)
    let matrixInv = Seq.map (fun (t:AffineTransform) -> t.GetInverse) transforms |> Seq.reduceBack (fun acc trans -> acc * trans)

    new AffineTransform (matrix, matrixInv, false)



(* Translate *)

let translate offset = 
    let matrix = Matrix.identity
    let ox, oy, oz = Vector.getCoord offset

    matrix.SetColumn (3, ox, oy, oz, 1.)


    let matrixInv = Matrix.identity

    matrixInv.SetColumn (3, -ox, -oy, -oz, 1.)

    new AffineTransform (matrix, matrixInv, false)


(* Scale *)

let scale amount =
    let matrix = Matrix.identity
    let (hxx, hyy, hzz) = Vector.getCoord amount

    matrix.Set 0 0 hxx
    matrix.Set 1 1 hyy
    matrix.Set 2 2 hzz
    

    let matrixInv = Matrix.identity
    let (hxx', hyy', hzz') = (1./hxx, 1./hyy, 1./hzz)

    matrixInv.Set 0 0 hxx'
    matrixInv.Set 1 1 hyy'
    matrixInv.Set 2 2 hzz'

    new AffineTransform (matrix, matrixInv, false)


(* Rotate X *)

let rotateX angle =
    let matrix = Matrix.identity
    matrix.SetColumn (1, 0., cos angle, sin angle, 0.)
    matrix.SetColumn (2, 0., -sin angle, cos angle, 0.)


    let matrixInv = Matrix.identity
    matrixInv.SetColumn (1, 0., cos angle, -sin angle, 0.)
    matrixInv.SetColumn (2, 0., sin angle, cos angle, 0.)

    new AffineTransform (matrix, matrixInv, false)


(* Rotate Y *)

let rotateY angle =
    let matrix = Matrix.identity
    matrix.SetColumn (0, cos angle, 0., -sin angle, 0.)
    matrix.SetColumn (2, sin angle, 0., cos angle, 0.)

    let matrixInv = Matrix.identity
    matrixInv.SetColumn (0, cos angle, 0., sin angle, 0.)
    matrixInv.SetColumn (2, -sin angle, 0., cos angle, 0.)

    new AffineTransform (matrix, matrixInv, false)


(* Rotate Z *)

let rotateZ angle =
    let matrix = Matrix.identity
    matrix.SetColumn (0, cos angle, sin angle, 0., 0.) 
    matrix.SetColumn (1, -sin angle, cos angle, 0., 0.)


    let matrixInv = Matrix.identity
    matrixInv.SetColumn (0, cos angle, -sin angle, 0., 0.) 
    matrixInv.SetColumn (1, sin angle, cos angle, 0., 0.)

    new AffineTransform (matrix, matrixInv, false)

(* Mirror X *)

let mirrorX () =
    let matrix = Matrix.identity
    matrix.SetColumn (0, -1., 0., 0., 0.)
    
    new AffineTransform (matrix, matrix, false)


(* Mirror Y *)

let mirrorY () =
    let matrix = Matrix.identity
    matrix.SetColumn (1, 0., -1., 0., 0.)

    new AffineTransform (matrix, matrix, false)


(* Mirror Z *)

let mirrorZ () =
    let matrix = Matrix.identity
    matrix.SetColumn (2, 0., 0., -1., 0.)

    new AffineTransform (matrix, matrix, false)


(* Sheare XY, XZ, YX, YZ, ZX, ZY *)

let sheare (hxy, hxz) (hyx, hyz) (hzx, hzy) =
    let matrix = Matrix.identity
    matrix.SetColumn (0, 1., hxy, hxz, 0.)
    matrix.SetColumn (1, hyx, 1., hyz, 0.)
    matrix.SetColumn (2, hzx, hzy, 1., 0.)

    let matrixInv = Matrix.identity
    matrixInv.SetColumn (0, 1.-hyx*hzy, -hxy+hxz*hzy, -hxz+hxy*hyz, 0.)
    matrixInv.SetColumn (1, -hyx+hyz*hzx, 1.-hxz*hzx, -hyz+hxz*hyx, 0.)
    matrixInv.SetColumn (2, -hzx+hyx*hzy, -hzy+hxy*hzx, 1.-hxy*hyx, 0.)
    let D = 1.-hxy*hyx-hxz*hzx-hyz*hzy+hxy*hyz*hzx+hxz*hyx*hzy
    matrixInv.SetColumn (3, 0., 0., 0., D)
    let matrixInvOut = matrixInv.MultScalar(1./D)

    AffineTransform(matrix, matrixInvOut, false)

let sheareXY a =
    sheare (a, 0.) (0., 0.) (0., 0.)

let sheareXZ a =
    sheare (0., a) (0., 0.) (0., 0.)

let sheareYX a =
    sheare (0., 0.) (a, 0.) (0., 0.)

let sheareYZ a =
    sheare (0., 0.) (0., a) (0., 0.)

let sheareZX a =
    sheare (0., 0.) (0., 0.) (a, 0.)

let sheareZY a =
    sheare (0., 0.) (0., 0.) (0., a)


  