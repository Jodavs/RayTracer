module BaseShape

open Point
open Vector
open Types
open Texture
open ManyDegree
open PolyEval
open PolySolve
open ExprParse
open ExprToPoly
open System
open Matrix
open AffineTransform
open Shape

type BoundingBox = Types.BoundingBox
// Type used for the kd-trees
type Tree (hitFunc, boundingBox) =
    inherit BaseShape ()

    let lower, upper = boundingBox
    override s.GetBoundingBox = boundingBox
    override s.HitBoundingBox (R(o,d)) = Shape.BBHitFunc o d lower upper <> None
    override s.HitFunc transNormFunc textures ray = hitFunc transNormFunc textures ray


type Sphere (radius) =
    inherit BaseShape ()
    // upper and lower bounds for the bounding box of the sphere
    let lower = mkPoint -radius -radius -radius
    let upper = mkPoint radius radius radius
    override s.GetBoundingBox = (lower, upper)

    override s.HitBoundingBox (R(o,d)) =       
        Shape.BBHitFunc o d lower upper <> None

    override s.HitFunc transNormFunc textures (R(o,d)) = 
        let (ox,oy,oz) = Point.getCoord o
        let (dx,dy,dz) = Vector.getCoord d

        let a = dx**2. + dy**2. + dz**2.
        let b = 2. * (ox*dx + oy*dy + oz*dz)
        let c = ox**2. + oy**2. + oz**2. - radius**2.

        let D = b**2. - (4. * a * c)
        // Get all solutions for the equation and sort from small to large (because we only want the solution with smallest x value)
        let solutions = List.sort (solveScnDgrPoly a b D)
        // checks if there is any solution for polynomial 
        if solutions.Length = 0 
        then
            None
        else 
            let dist = solutions.Head
            let hitPoint = o + dist*d
            let (px, py, pz) = Point.getCoord hitPoint

            let norm = mkVector (px/radius) (py/radius) (pz/radius)
            let transNorm = transNormFunc norm

            // Texturing
            let nx, ny, nz = Vector.getCoord norm
            // Calculate angle from vertical axis
            let theta = Math.Acos(ny)
            // Calculate angle from horizontal (z) axis
            let phi' = Math.Atan2 ( nx , nz )
            let phi = if phi' < 0.
                      then phi' + 2. * Math.PI
                      else phi'
            // Calculate texture coordinates
            let u = phi / (Math.PI*2.)
            let v = 1. - theta/Math.PI

            let mat = textures.[0].GetMaterial u v


            let hit = {distance = dist; normal = transNorm; material = mat;}
            Some(hit)

type Box (p1, p2) =
    inherit BaseShape ()

    let width = abs(Point.getX p1 - Point.getX p2)
    let height = abs(Point.getY p1 - Point.getY p2)
    let depth = abs(Point.getZ p1 - Point.getZ p2)

    override s.GetBoundingBox = (p1, p2)

    override s.HitBoundingBox ray = true // Doesn't make sense to have a bounding box on a box

    override s.HitFunc transNormFunc textures (R(o,d)) = 
        // Get the coordinate and direction of the ray
        let ox, oy, oz = Point.getCoord o
        let dx, dy, dz = Vector.getCoord d
        // Get box coordinates
        let Lx, Ly, Lz = Point.getCoord p1
        let Hx, Hy, Hz = Point.getCoord p2

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

        // Check if we even hit the box
        if t >= t' || t' < 0. then None
        else
            
            let getX p = (Point.getX p - Lx) / width
            let getY p = (Point.getY p - Ly) / height 
            let getZ p = (Point.getZ p - Lz) / depth 
            // Calculate the normal by checking which side of the box is hit first
            let norm, coordA, coordB, texture = 
                if t > 0. then
                    if   tx >= ty && tx >= tz then 
                        if dx > 0. then (mkVector 1. 0. 0., getY, getZ, textures.[4]) // Left
                                   else (mkVector 1. 0. 0., getY, getZ, textures.[5]) // Right
                    elif ty >= tx && ty >= tz then 
                        if dy > 0. then (mkVector 0. 1. 0., getX, getZ, textures.[3]) // Bottom
                                   else (mkVector 0. 1. 0., getX, getZ, textures.[2]) // Top
                    else 
                        if dz > 0. then (mkVector 0. 0. 1., getX, getY, textures.[1]) // Back
                                   else (mkVector 0. 0. 1., getX, getY, textures.[0]) // Front
                else // 0: Front, 1: Back, 2: Top, 3: Bottom, 4: Left, 5: Right
                    if   tx' <= ty' && tx' <= tz then (mkVector 1. 0. 0., getZ, getY, textures.[5]) // Right
                    elif ty' <= tx' && ty' <= tz then (mkVector 0. 1. 0., getX, getZ, textures.[3]) // Bottom
                    else                              (mkVector 0. 0. 1., getX, getY, textures.[1]) // Back

            let transNorm = transNormFunc norm

            // Return the calculated values
            let dist = if t > 0. then t else t'
            let p = o + dist*d

            let mat = texture.GetMaterial (coordA p) (coordB p)
            //let mat = RGB(mkColour 0. 0. 1., 0.)
            Some({distance = dist; normal = transNorm; material = mat})

type Disk (radius) =
    inherit BaseShape ()
    let lower = mkPoint -radius -radius -Shape.eps
    let upper = mkPoint radius radius Shape.eps
    override s.GetBoundingBox = (lower, upper)
    override s.HitBoundingBox ray = true

    override s.HitFunc transNormFunc textures (R(o,d)) =
        let oz = Point.getZ o
        let dz = Vector.getZ d
    
        let dist = -(oz/dz)
        if dz <> 0. && dist > 0.
        then
            let hitPoint = o + dist*d
            let (px,py,_) = Point.getCoord hitPoint
            if px**2. + py**2. <= radius**2.
            then 
                // Make normal vector
                let norm = mkVector 0. 0. 1.
                let transNorm = transNormFunc norm

                // Calculate texture coordinates
                let u = (px+radius)/(2.*radius)
                let v = (py+radius)/(2.*radius)

                let mat = textures.[0].GetMaterial u v

                let hit = {distance = dist; normal = transNorm; material = mat;}
                Some(hit)
            else None
        else None

type HollowCylinder (radius, height) = 
    inherit BaseShape ()
    let lower = mkPoint -radius -(height/2.) -radius
    let upper = mkPoint radius (height/2.) radius
    override s.GetBoundingBox = (lower, upper)

    override s.HitBoundingBox (R(o,d)) =
        Shape.BBHitFunc o d lower upper <> None

    override s.HitFunc transNormFunc textures (R(o,d)) =
        let (ox,_,oz) = Point.getCoord o
        let (dx,_,dz) = Vector.getCoord d
        
        let a = dx**2. + dz**2.
        let b = 2. * (ox*dx + oz*dz)
        let c = ox**2. + oz**2. - radius**2.

        let D = b**2. - (4. * a * c)
        let solutions = solveScnDgrPoly a b D

        let solutionToHit dist = 
            let hitPoint = o + dist*d
            let (px, py, pz) = Point.getCoord hitPoint
            if py > (-height/2.) && py < (height/2.) then
                // Make normal vector
                let norm = mkVector (px/radius) 0. (pz/radius)
                let transNorm = transNormFunc norm

                // Calculate texture coordinates
                let nx, _, nz = Vector.getCoord norm
                let phi' = Math.Atan2(nx, nz)
                let phi = if phi' < 0.
                          then phi' + 2. * Math.PI
                          else phi'
                let u = phi/(2.*Math.PI)
                let v = py/height + 0.5
                let mat = textures.[0].GetMaterial u v

                let hit = {distance = dist; normal = transNorm; material = mat;}
                Some(hit)
            else None

        let hits = List.fold (fun acc e -> if (solutionToHit e).IsSome then (solutionToHit e).Value::acc else acc) [] solutions
        let hitsSorted = List.sortBy (fun e -> e.distance) hits
        if hitsSorted.Length > 0 then Some(hitsSorted.Head)
        else None

type InfinitePlane (textureScale: float) =
    inherit BaseShape ()
    override s.GetBoundingBox = (mkPoint Double.MinValue Double.MinValue -Shape.eps, mkPoint Double.MaxValue Double.MaxValue Shape.eps)
    override s.HitBoundingBox ray = true // Infinite plane shouldn't have a bounding box
    override s.HitFunc transNormFunc textures (R(o,d)) =
        let oz = Point.getZ o
        let dz = Vector.getZ d

        let dist = -(oz/dz)
    
        if dist > 0.
        then
            let hitPoint = o + dist*d

            let norm = mkVector 0. 0. 1.
            let transNorm = transNormFunc norm

            let px, py, _ = Point.getCoord hitPoint
            let spx, spy = (abs(px/textureScale), abs(py/textureScale))
            let fu, fv = ( spx - (float (int spx)), spy - (float (int spy)) )
            let u = if px < 0. then 1.-fu else fu
            let v = if py < 0. then 1.-fv else fv 
            let mat = textures.[0].GetMaterial u v

            let hit = {distance = dist; normal = transNorm; material = mat;}
            Some(hit)
        else 
            None


type ImplicitSurface (str:string, bounds:BoundingBox) =
    inherit BaseShape ()
    let expr = parseStr str
    let poly = genRayPoly str
    let derived = derive poly

    let normPolyx = stringToPoly str "x" |> derive
    let normPolyy = stringToPoly str "y" |> derive
    let normPolyz = stringToPoly str "z" |> derive

    override s.GetBoundingBox = bounds
    override s.HitBoundingBox (R(o, d)) = 
        let lower, upper = bounds

        Shape.BBHitFunc o d lower upper <> None

    override s.HitFunc transNormFunc texture (R(o,d) as ray) =
        match getMinRoot poly derived ray 10000. with
        | Some(t) -> 
             let hitPoint = o + t*d
             let x, y, z = Point.getCoord hitPoint
             let envMap = Map.ofList [("x", x); ("y", y); ("z", z)]

             let res = eval envMap expr
             if res > 1.0 || res < -1.0 then None
             else 
                 let normX = evalPolyMap normPolyx envMap x
                 let normY = evalPolyMap normPolyy envMap y
                 let normZ = evalPolyMap normPolyz envMap z

                 let transformedNorm = transNormFunc (mkVector normX normY normZ) |> normalise

                 let hit = {distance = t; normal = transformedNorm; material = texture.[0].GetMaterial 0. 0.}
                 Some(hit)
        | None ->  None



type Triangle (a:Point, b:Point, c:Point, norma: Vector, normb: Vector, normc: Vector, faceNormal: Vector, smooth:bool, uCoords: float list, vCoords: float list) =
    inherit BaseShape ()
    // getting points used for finding the lower upper points of the bounding box
    let ax, ay, az = Point.getCoord a
    let bx, by, bz = Point.getCoord b
    let cx, cy, cz = Point.getCoord c

    let a1x = ax - bx
    let b1y = ax - cx
    let a2x = ay - by
    let b2y = ay - cy
    let a3x = az - bz
    let b3y = az - cz
    
    // finding lower and upper points for the triangle bounding box
    let lower = mkPoint ((min ax bx |> min cx)-Shape.eps) ((min ay by |> min cy)-Shape.eps) ((min az bz |> min cz)-Shape.eps)
    let upper = mkPoint ((max ax bx |> max cx)+Shape.eps) ((max ay by |> max cy)+Shape.eps) ((max az bz |> max cz)+Shape.eps)
    override s.GetBoundingBox = (lower, upper)

    override s.HitBoundingBox (R(o,d)) = true
        //Shape.BBHitFunc o d lower upper <> None

    override s.HitFunc transNormFunc textures (R(o, d)) =
        //let a, b, c = getVerticesFromFace vertices face
        // Get the points for the ray origin point and direction vector
        let ox, oy, oz = Point.getCoord o
        let dx, dy, dz = Vector.getCoord d
        // Values used for cramer's rule 
        let c1z = dx
        let d1  = ax - ox        
        let c2z = dy
        let d2 = ay - oy
        let c3z = dz
        let d3 = az - oz
        // Calculating the determinat using Cramer's rule
        let D = (a1x*(b2y * c3z - c2z * b3y)) + (b1y*(c2z * a3x - a2x * c3z)) + (c1z*(a2x * b3y - b2y * a3x))
        // Finding beta, gamma and t which corresponds to points of x, y and z respectivly
        let beta = ((d1*(b2y * c3z - c2z * b3y)) + (b1y*(c2z * d3 - d2 * c3z)) + (c1z*(d2 * b3y - b2y * d3)))/D
        let gamma = ((a1x*(d2 * c3z - c2z * d3)) + (d1*(c2z * a3x - a2x * c3z)) + (c1z*(a2x * d3 - d2 * a3x)))/D
        let t = ((a1x*(b2y * d3 - d2 * b3y)) + (b1y*(d2 * a3x - a2x * d3)) + (d1*(a2x * b3y - b2y * a3x)))/D
        
        if beta < 0. || gamma < 0. || beta+gamma > 1. || t <= 0.000001 then None
        else
            // calculating alpha, which is used for texturing and smooth shading
            let alpha = 1. - beta - gamma
            // the normal used for smooth shading
            let realnorm = 
                if smooth then      
                    // calculating the normal for smooth shading
                    let vnorm = alpha*norma + beta*normb + gamma*normc
                    transNormFunc (Vector.normalise vnorm)
                else 
                    // Uses the triangles normals, used when smoothing shading is not needed.
                    transNormFunc faceNormal

            let hp = o + t*d
            // Calculating the texture coordinates of the hit point 
            let u = alpha*uCoords.[0] + beta*uCoords.[1] + gamma*uCoords.[2]
            let v = alpha*vCoords.[0] + beta*vCoords.[1] + gamma*vCoords.[2]
            // gets the material of the texture
            let mat = textures.[0].GetMaterial u v
            // creates the hit for the some
            let hit = {distance = t; normal = realnorm; material = mat}
            Some(hit)



let mkSolidCylinder (center:Point) (radius:float) (height:float) (baseTexture : Texture) (top : Texture) (bottom : Texture) =
    let disk = new Disk (radius)

    let cylinder = new Shape(new HollowCylinder (radius, height), [baseTexture], AffineTransform.identity)
    let topDisk  = new Shape(disk, [top], compose [rotateX (Math.PI/2.); translate (mkVector 0. (height*0.5) 0.)])
    let botDisk  = new Shape(disk, [bottom], compose [rotateX (Math.PI/2.); translate (mkVector 0. (-height*0.5) 0.)])
    
    let transform = AffineTransform.translate (mkVector ((Point.getX center)) ((Point.getY center)) ((Point.getZ center)))

    new Group([topDisk; botDisk; cylinder], transform)

type Rectangle (width, height) =
     inherit BaseShape ()
     let lower = mkPoint (-width) (-height) -Shape.eps
     let upper = mkPoint (width) (height) Shape.eps
     override s.GetBoundingBox = (lower, upper)
     override s.HitBoundingBox ray = true
     override s.HitFunc transNormFunc textures (R(o,d)) =
         let oz = Point.getZ o
         let dz = Vector.getZ d
     
         let dist = -(oz/dz)
         if dz <> 0. && dist > 0.
         then
             let hitPoint = o + dist*d
             let (px,py,_) = Point.getCoord hitPoint
             let ax, ay = -width/2., -height/2.
 
             if ax > px || px > ax+width || ay > py || py > ay+height
             then None
             else
                 // Make normal vector
                 let norm = mkVector 0. 0. 1.
                 let transNorm = transNormFunc norm 
 
                 // Calculate texture coordinates
                 let u = (px-ax)/width
                 let v = (py-ay)/height
 
                 let mat = textures.[0].GetMaterial u v
 
                 let hit = {distance = dist; normal = transNorm; material = mat;}
                 Some(hit)
         else None


open Colour

let mkRectFromPoint origin bottomRight topLeft texture = 
    let u = Point.distance origin bottomRight
    let v = Point.distance origin topLeft
    let u' = Vector.normalise u
    let v' = Vector.normalise v
    let ux, uy, uz = u' |> Vector.getCoord
    let vx, vy, vz = v' |> Vector.getCoord
    let wx, wy, wz = u' % v' |> Vector.getCoord

    let mat = Matrix.identity
    mat.SetColumn(0, ux, uy, uz, 0.)
    mat.SetColumn(1, vx, vy, vz, 0.)
    mat.SetColumn(2, wx, wy, wz, 0.)

    let matInv = Matrix.identity
    matInv.SetColumn(0, ux, vx, wx, 0.)
    matInv.SetColumn(1, uy, vy, wy, 0.)
    matInv.SetColumn(2, uz, vz, wz, 0.)

    let ox, oy, oz = Point.getCoord origin
    let width = Vector.magnitude u
    let height = Vector.magnitude v
    let originVector = (mkVector ox oy oz)

    let pretransform = (mkVector (width/2.) (height/2.) 0.)

    let trans = AffineTransform.compose [translate pretransform; AffineTransform(mat, matInv, false); translate originVector]

    Shape(Rectangle(Vector.magnitude u, Vector.magnitude v), texture, trans)