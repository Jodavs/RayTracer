module RayTracer

open Point
open Colour
open Vector
open System
open Types
open Shape

(*type AmbientLight = Colour * float
type Light = {position: Point; colour: Colour; intensity: float}

type OrthFrame = Vector * Vector * Vector

type Camera = {position: Point; lookat: Point;
                up: Vector; zoom: float;
                width: float; height: float;
                resX: int; resY: int }*)

type Scene = {shapes: AbstractShape list; lights: Light list; ambientLight: AmbientLight; maxReflections: int}

//val getClosestHit : Ray -> Shape list -> Hit option

//val mkShadowRay : hitPoint:Point -> hitNorm:Vector -> lightPos:Point -> Ray

val renderPixel : scene:Scene -> (float -> float -> Ray) -> x:int -> y:int -> System.Drawing.Color
val renderAsync : scene:Scene -> camera:Camera -> renderSettings:Map<String, int> -> string -> unit
