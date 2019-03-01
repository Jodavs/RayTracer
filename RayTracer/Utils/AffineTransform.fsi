module AffineTransform

open Types
open Vector
open Matrix

type AffineTransform =
    new : Matrix * Matrix * bool -> AffineTransform
    member Get : Matrix
    member GetInverse : Matrix
    member IsIdentity : bool

val compose      : AffineTransform seq -> AffineTransform
val translate    : Vector -> AffineTransform
val scale        : Vector -> AffineTransform
val rotateX      : float -> AffineTransform
val rotateY      : float -> AffineTransform
val rotateZ      : float -> AffineTransform
val mirrorX      : unit  -> AffineTransform
val mirrorY      : unit  -> AffineTransform
val mirrorZ      : unit  -> AffineTransform
val sheare       : float*float -> float*float -> float*float -> AffineTransform
val sheareXY     : float -> AffineTransform
val sheareXZ     : float -> AffineTransform
val sheareYX     : float -> AffineTransform
val sheareYZ     : float -> AffineTransform
val sheareZX     : float -> AffineTransform
val sheareZY     : float -> AffineTransform 
val identity : AffineTransform