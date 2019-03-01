module Point

type Vector = Vector.Vector
type Point =
  | P of float * float * float
  override p.ToString() =
    match p with
      P(x,y,z) -> "("+x.ToString()+","+y.ToString()+","+z.ToString()+")"

let mkPoint x y z = P(x,y,z)
let getX (P(x,_,_)) = x
let getY (P(_,y,_)) = y
let getZ (P(_,_,z)) = z
let getCoord (P(x,y,z)) = (x,y,z)
let move (P(px,py,pz)) (v:Vector) = P(px+Vector.getX(v), py+Vector.getY(v), pz+Vector.getZ(v))
let distance (P(px,py,pz)) (P(qx,qy,qz)) = Vector.mkVector (qx-px) (qy-py) (qz-pz)
let direction p q = Vector.normalise(distance p q)
let round (P(px,py,pz)) (d:int) = P(System.Math.Round(px,d),System.Math.Round(py,d),System.Math.Round(pz,d))

type Point with
  static member ( + ) (P(px,py,pz), v:Vector) : Point = P(px+Vector.getX(v), py+Vector.getY(v), pz+Vector.getZ(v))
  static member ( - ) ((P(p1x,p1y,p1z)), (P(p2x,p2y,p2z))) = Vector.mkVector (p1x-p2x) (p1y-p2y) (p1z-p2z)
