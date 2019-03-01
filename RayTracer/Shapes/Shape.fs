module Shape

open Types
open Vector
open Point
open Matrix
open PolySolve
open AffineTransform
open System
open Colour
open ManyDegree
open ExprToPoly
open PolyEval
open Texture


[<AbstractClass>]
type BaseShape () =
    abstract member HitFunc : (Vector -> Vector) -> Texture list -> Ray -> Hit Option
    // Called ONCE to generate the bounding box based on the transformation carried by the parent Shape
    abstract member GetBoundingBox : BoundingBox
    abstract member HitBoundingBox : Ray -> bool

[<AbstractClass>]
type AbstractShape () =
    abstract member HitFunc : Ray -> Hit option
    abstract member GetBoundingBox : unit -> BoundingBox
    abstract member HitBoundingBox : Ray -> bool
    abstract member IsInside : Point -> bool
    abstract member ApplyTransform : AffineTransform -> AbstractShape
  
type Shape (baseShape: BaseShape, textures: Texture list, transform: AffineTransform) =
    inherit AbstractShape ()
    static member eps = 0.001

    member s.baseShape = baseShape
    member s.textures  = textures
    member s.transform = transform

    override s.ApplyTransform trans =
        new Shape(baseShape, textures, compose [s.transform; trans]) :> AbstractShape

    override s.HitFunc ray = 
        let invTrans = transform.GetInverse
        match transform.IsIdentity with
        | true  -> baseShape.HitFunc invTrans.MultVector textures ray // Don't transform the Shape if it is an identity matrix. Save the calculations!
        | false -> baseShape.HitFunc (invTrans.Transpose.MultVector >> Vector.normalise) textures (invTrans.MultRay ray)

    override s.GetBoundingBox () =
        let baseBB = baseShape.GetBoundingBox

        match transform.IsIdentity with
        | true -> baseBB // Save the calculations, as they don't matter for non-transformed shapes! :D
        | false ->
            let lower, upper = baseBB
            let (Lx, Ly, Lz) = Point.getCoord lower
            let (Hx, Hy, Hz) = Point.getCoord upper

            let pList = [mkPoint Lx Ly Lz; mkPoint Lx Ly Hz; mkPoint Lx Hy Lz; mkPoint Lx Hy Hz; mkPoint Hx Ly Lz; mkPoint Hx Ly Hz; mkPoint Hx Hy Lz; mkPoint Hx Hy Hz]

            let transList = List.map transform.Get.MultPoint pList

            let Lx = List.fold (fun acc point -> min (Point.getX point) acc) Double.MaxValue transList
            let Ly = List.fold (fun acc point -> min (Point.getY point) acc) Double.MaxValue transList
            let Lz = List.fold (fun acc point -> min (Point.getZ point) acc) Double.MaxValue transList

            let Hx = List.fold (fun acc point -> max (Point.getX point) acc) Double.MinValue transList
            let Hy = List.fold (fun acc point -> max (Point.getY point) acc) Double.MinValue transList
            let Hz = List.fold (fun acc point -> max (Point.getZ point) acc) Double.MinValue transList
            
            printfn "%A ASDSA" (mkPoint Lx Ly Lz, mkPoint Hx Hy Hz)

            (mkPoint Lx Ly Lz, mkPoint Hx Hy Hz)

    override s.HitBoundingBox ray =
        match transform.IsIdentity with
        | true  -> baseShape.HitBoundingBox ray // Don't transform the Shape if it is an identity matrix. Save the calculations!
        | false -> baseShape.HitBoundingBox (transform.GetInverse.MultRay ray)

    override s.IsInside point =
        let dir = mkVector 0. 0. 1.
        let startRay = R(point, dir)

        let rec inner (R(o,d) as ray) isInside =
            match s.HitFunc ray with
            | Some hit ->
                let movedPoint = Point.move o ((hit.distance * d) + (Shape.eps*dir))
                let ray2 = R(movedPoint, dir)
                inner ray2 (not isInside)
            | None ->
                isInside
        inner startRay false

    static member GetClosestHit (ray:Ray) shapes =
        List.fold (fun acc (shape:AbstractShape) -> 
            if shape.HitBoundingBox ray then
                match shape.HitFunc ray with
                | None -> acc
                | Some hit -> if acc = None || hit.distance < acc.Value.distance then Some(hit) else acc
            else acc
        ) None shapes
  
    static member BBHitFunc o d lower upper =
        let (ox,oy,oz) = Point.getCoord o
        let (dx,dy,dz) = Vector.getCoord d
        let Lx, Ly, Lz = Point.getCoord lower
        let Hx, Hy, Hz = Point.getCoord upper


        // Helper function for calculating the intersections with the sides of the box
        let hdist d o L H =
            if d >= 0. then ((L - o)/d, (H - o)/d)
            else            ((H - o)/d, (L - o)/d)

        // Actually calculate the distances
        let tx, tx' = hdist dx ox Lx Hx
        let ty, ty' = hdist dy oy Ly Hy
        let tz, tz' = hdist dz oz Lz Hz
    
        // Find the biggest t and smallest t'
        let t  = max tx  ty  |> max tz
        let t' = min tx' ty' |> min tz'

        if t < t' && t' > 0. then
            Some((t, t'))
        else
            None


    /// Cheap hit func used to calculate only the essential information needed for shadow rays
    static member MakeShadowRay hitPoint (hitNormal: Vector) lightPoint offset =
        let dirLight = direction hitPoint lightPoint
        let movedPoint = hitPoint + (offset*hitNormal)
        R(movedPoint, dirLight)

type Group (shapes: AbstractShape list, transform: AffineTransform) =
    inherit AbstractShape ()
    
    // Get all lower and upper points of all shapes
    let minPoints, maxPoints = List.map (fun (s:AbstractShape) -> s.GetBoundingBox()) shapes |> List.unzip

    // Find the lowest of all minPoints
    let Lx = List.fold (fun acc point -> min (Point.getX point) acc) Double.MaxValue minPoints
    let Ly = List.fold (fun acc point -> min (Point.getY point) acc) Double.MaxValue minPoints
    let Lz = List.fold (fun acc point -> min (Point.getZ point) acc) Double.MaxValue minPoints

    // Find the highest of all maxPoints
    let Hx = List.fold (fun acc point -> max (Point.getX point) acc) Double.MinValue maxPoints
    let Hy = List.fold (fun acc point -> max (Point.getY point) acc) Double.MinValue maxPoints
    let Hz = List.fold (fun acc point -> max (Point.getZ point) acc) Double.MinValue maxPoints
        
    let lower = mkPoint Lx Ly Lz
    let upper = mkPoint Hx Hy Hz


    member s.shapes = shapes
    member s.transform = transform

    override s.ApplyTransform trans =
        // As the transNorm of this group cannot be passed to the Shapes, we must recursively apply the transform to all shapes.
        let newShapes = List.map (fun (ss:AbstractShape)-> ss.ApplyTransform trans) shapes
        new Group(newShapes, AffineTransform.identity) :> AbstractShape

    override s.HitFunc inputRay = 
        let ray = transform.GetInverse.MultRay inputRay
        Shape.GetClosestHit ray shapes

    override s.GetBoundingBox () =
        (lower, upper)

    override s.HitBoundingBox (R(o, d)) = true
        //let (R(o, d)) = transform.GetInverse.MultRay inputRay
        //Shape.BBHitFunc o d lower upper <> None

    override s.IsInside point =
        List.exists (fun (s:AbstractShape) -> s.IsInside point) shapes
        