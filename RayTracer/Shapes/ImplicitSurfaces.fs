module ImplicitSurfaces

open Point
open Shape
open BaseShape

let makeTorus ringRadius tubeRadius =
    let outerRadius = ringRadius + tubeRadius + Shape.eps
    let halfDepth = tubeRadius + Shape.eps
    let box = (mkPoint -outerRadius -outerRadius -halfDepth, mkPoint outerRadius outerRadius halfDepth)
    let poly = "(x^2+y^2+z^2+" + string(ringRadius**2.) + "+-" + string(tubeRadius**2.) + ")^2+-" + string(4.*ringRadius**2.) + "(x^2+y^2)"
    printfn "%A" poly
    ImplicitSurface(poly, box)