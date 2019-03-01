module CSG

open Types
open Shape
open BaseShape

type Union (shape1 : AbstractShape, shape2 : AbstractShape) =
    inherit Group ([shape1; shape2], AffineTransform.identity)

    override s.HitFunc (R(originalP, _) as inputRay) =

        let rec inner (R(point, dir) as transRay) =
            let hit1 = shape1.HitFunc transRay
            let hit2 = shape2.HitFunc transRay

          

            match hit1, hit2 with
            | Some h1, Some h2 ->
                if h1.distance < h2.distance then
                    let hitpoint = Point.move point (h1.distance*dir)
                    if shape2.IsInside(hitpoint) then
                        let movedPoint = Point.move hitpoint (Shape.eps*dir)
                        let transRay = R(movedPoint, dir)
                        inner transRay
                    else
                        let remainingDist = Point.distance originalP point |> Vector.magnitude
                        Some {distance = h1.distance + remainingDist; normal = h1.normal; material = h1.material}
                else
                    let hitpoint = Point.move point (h2.distance*dir)
                    if shape1.IsInside(hitpoint) then
                        let movedPoint = Point.move hitpoint (Shape.eps*dir)
                        let transRay = R(movedPoint, dir)
                        inner transRay
                    else
                        let remainingDist = Point.distance originalP point |> Vector.magnitude
                        Some {distance = h2.distance + remainingDist; normal = h2.normal; material = h2.material}
            | Some h1, None ->
                let remainingDist = Point.distance originalP point |> Vector.magnitude
                Some {distance = h1.distance + remainingDist; normal = h1.normal; material = h1.material}
            | None, Some h2 ->
                let remainingDist = Point.distance originalP point |> Vector.magnitude
                Some {distance = h2.distance + remainingDist; normal = h2.normal; material = h2.material}
            | None, None ->
                None
        inner inputRay


type Intersect (shape1 : AbstractShape, shape2 : AbstractShape) =
    inherit Group ([shape1; shape2], AffineTransform.identity)

    override s.IsInside point =
        List.forall (fun (s:AbstractShape) -> s.IsInside point) [shape1; shape2]

    override s.HitFunc (R(o,d) as ray) =
        let hit1 = shape1.HitFunc ray
        let hit2 = shape2.HitFunc ray
 
        match hit1, hit2 with
        | Some h1, Some h2 ->
            let h1Moved = Point.move o (h1.distance*d)
            let h2Moved = Point.move o (h2.distance*d)

            if shape2.IsInside(h1Moved) then
                hit1
            elif shape1.IsInside(h2Moved) then
                hit2
            else
                None
        | _, _ -> None


type Subtract (shape1 : AbstractShape, shape2 : AbstractShape) =
    inherit Group ([shape1; shape2], AffineTransform.identity)

    //override s.GetBoundingBox () = shape1.GetBoundingBox()

    override s.IsInside point =
        shape1.IsInside point && not (shape2.IsInside point)

    override s.HitFunc (R(point, dir) as ray) =
        
        let hit1 = shape1.HitFunc ray

        match hit1 with
        | Some h1 ->
            let hitPoint = Point.move point (h1.distance*dir)
            if shape2.IsInside(hitPoint) then
                let movedPoint = Point.move hitPoint (Shape.eps*dir)
                let newRay = R(movedPoint, dir)
                let hit2 = shape2.HitFunc newRay

                match hit2 with
                | Some h2 ->
                    let hitPoint2 = Point.move movedPoint (h2.distance*dir)
                    if shape1.IsInside(hitPoint2) then
                        Some {distance = h1.distance + h2.distance; normal = h2.normal; material = h2.material}
                    else
                        None
                | None ->
                    None
            else
                hit1
        | None ->
            None