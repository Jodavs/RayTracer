module Types

open Vector
open Colour
open Point

type Ray = R of Point * Vector
type Material = RGB of Colour * float
type Hit = {distance: float; normal: Vector; material: Material}

type AmbientLight = Colour * float
type Light = {position: Point; colour: Colour; intensity: float}

type OrthFrame = Vector * Vector * Vector

type Camera = {position: Point; lookat: Point;
                up: Vector; zoom: float;
                width: float; height: float;
                resX: int; resY: int }

type RenderImageDelegate = delegate of (int * int * System.Drawing.Color) list -> unit

type BoundingBox = Point * Point

let unionBB (alow, ahigh) (blow, bhigh) = 
    let alow_x, alow_y, alow_z = Point.getCoord alow
    let ahigh_x, ahigh_y, ahigh_z = Point.getCoord ahigh
    let blow_x, blow_y, blow_z = Point.getCoord alow
    let bhigh_x, bhigh_y, bhigh_z = Point.getCoord ahigh
    (mkPoint (min alow_x blow_x) (min alow_y blow_y) (min alow_z blow_z), mkPoint (max ahigh_x bhigh_x) (max ahigh_y bhigh_y) (max ahigh_z bhigh_z))