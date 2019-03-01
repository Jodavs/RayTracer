namespace Tracer

open Vector
open Point
open Colour
open Types
open Shape
open CSG
open BaseShape
open RayTracer
open Matrix
open AffineTransform
open Texture
open PlyParser
open Mesh
open KDShapeTree
open KDMeshTree
open System


module API = 
  type dummy = unit

  type vector = Vector.Vector
  type point = Point.Point
  type colour = Colour.Colour
  type material = Types.Material
  type shape = Shape.AbstractShape
  type baseShape = Shape.BaseShape
  type texture = Texture.Texture
  type camera = Types.Camera
  type scene = RayTracer.Scene
  type light = Types.Light
  type ambientLight = Types.AmbientLight
  type transformation = AffineTransform.AffineTransform

  let mkVector (x : float) (y : float) (z : float) : vector = mkVector x y z
  let mkPoint (x : float) (y : float) (z : float) : point = mkPoint x y z
  let fromColor (c : System.Drawing.Color) : colour = Colour.fromColor c
  let mkColour (r : float) (g : float) (b : float) : colour = mkColour r g b

  let mkMaterial (c : colour) (r : float) : material = RGB (c, r)
  let mkTexture (f : float -> float -> material) : texture = TextureFunction(f) :> Texture
  let mkMatTexture (m : material) : texture = ColourTexture(m) :> Texture

  let mkShape (b : baseShape) (t : texture) : shape = new Shape(b, [t], AffineTransform.identity) :> AbstractShape
  let mkSphere (p : point) (r : float) (m : texture) : shape = new Shape(new Sphere (r), [m], AffineTransform.translate (mkVector (Point.getX p) (Point.getY p) (Point.getZ p))) :> AbstractShape
  let mkRectangle (bottomLeft : point) (topLeft : point) (bottomRight : point)  (t : texture) : shape
    = mkRectFromPoint bottomLeft bottomRight topLeft [t] :> AbstractShape
  let mkTriangle (a:point) (b:point) (c:point) (m : material) : shape = failwith "mkTriangle not implemented"
  let mkPlane (m : texture) : shape = new Shape(new InfinitePlane(1.), [m], AffineTransform.identity) :> AbstractShape
  let mkImplicit (s : string) : baseShape = new ImplicitSurface(s, (mkPoint Double.MinValue Double.MinValue Double.MinValue, mkPoint Double.MaxValue Double.MaxValue Double.MaxValue)) :> BaseShape
  let mkPLY (filename : string) (smooth : bool) : baseShape = parsePly filename smooth |> genTriangleList |> mkMeshKDTree

  let mkHollowCylinder (c : point) (r : float) (h : float) (t : texture) : shape = new Shape(new HollowCylinder (r, h), [t], AffineTransform.translate (mkVector (Point.getX c) (Point.getY c) (Point.getZ c))) :> AbstractShape
  let mkSolidCylinder (c : point) (r : float) (h : float) (t : texture) (top : texture) (bottom : texture) : shape
      = mkSolidCylinder c r h t top bottom :> AbstractShape
  let mkDisc (c : point) (r : float) (t : texture) : shape = new Shape(new Disk(r), [t], AffineTransform.translate (mkVector (Point.getX c) (Point.getY c) (Point.getZ c))) :> AbstractShape
  let mkBox (low : point) (high : point) (front : texture) (back : texture) (top : texture) (bottom : texture) (left : texture) (right : texture) : shape
      = new Shape(new Box (low, high), [front; back; top; bottom; left; right], AffineTransform.identity) :> AbstractShape

  let group (s1 : shape) (s2 : shape) : shape = new Group([s1; s2], AffineTransform.identity) :> AbstractShape
  let union (s1 : shape) (s2 : shape) : shape = new Union(s1, s2) :> AbstractShape
  let intersection (s1 : shape) (s2 : shape) : shape = new Intersect(s1, s2) :> AbstractShape
  let subtraction (s1 : shape) (s2 : shape) : shape = new Subtract(s1, s2) :> AbstractShape

  let mkCamera (pos : point) (look : point) (up : vector) (zoom : float) (width : float)
    (height : float) (pwidth : int) (pheight : int) : camera = {position = pos; lookat = look; up = up; zoom = zoom; width = width; height = height; resX = pwidth; resY = pheight}
  let mkThinLensCamera (pos : point) (look : point) (up : vector) (zoom : float) (width : float)
    (height : float) (pwidth : int) (pheight : int) (lensRadius : float) (fpDistance : float) : camera = failwith "mkCamera not implemented"
  
  let mkLight (p : point) (c : colour) (i : float) : light = {position = p; colour = c; intensity = i}
  let mkAmbientLight (c : colour) (i : float) : ambientLight = (c, i)

  let mkScene (s : shape list) (l : light list) (a : ambientLight) (m : int) : scene = {shapes = s; lights = l; ambientLight = a; maxReflections = m}
  let renderToScreen (sc : scene) (c : camera) : unit =
        let renderSettings = [ ("render_blocksize", 64) ] |> Map.ofSeq
        RayTracer.renderAsync sc c renderSettings ""
  let renderToFile (sc : scene) (c : camera) (path : string) : unit =
        let renderSettings = [ ("render_blocksize", 64) ] |> Map.ofSeq
        RayTracer.renderAsync sc c renderSettings path

  let translate (x : float) (y : float) (z : float) : transformation = translate (mkVector x y z)
  let rotateX (angle : float) : transformation = rotateX angle
  let rotateY (angle : float) : transformation = rotateY angle
  let rotateZ (angle : float) : transformation = rotateZ angle
  let sheareXY (distance : float) : transformation = sheareXY distance
  let sheareXZ (distance : float) : transformation = sheareXZ distance
  let sheareYX (distance : float) : transformation = sheareYX distance
  let sheareYZ (distance : float) : transformation = sheareYZ distance
  let sheareZX (distance : float) : transformation = sheareZX distance
  let sheareZY (distance : float) : transformation = sheareZY distance
  let scale (x : float) (y : float) (z : float) : transformation = scale (mkVector x y z)
  let mirrorY : transformation = mirrorY ()
  let mirrorZ : transformation = mirrorZ ()
  let mirrorX : transformation = mirrorX ()
  let mergeTransformations (ts : transformation list) : transformation = AffineTransform.compose ts
  let transform (sh : shape) (tr : transformation) : shape = sh.ApplyTransform tr

