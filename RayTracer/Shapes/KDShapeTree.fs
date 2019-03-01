module KDShapeTree

open System
open Types
open Point
open Shape
open BaseShape
open AffineTransform
open Texture
open Vector


type Split = int * float

type KDTreeNode =
    | Node of Split * KDTreeNode * KDTreeNode
    | Leaf of AbstractShape list

type KDTree = KDTreeNode * BoundingBox

let kdEps = 1.0E-6

let private getPointAxis axis point = 
    match axis with
    | 0 -> Point.getX point                   
    | 1 -> Point.getY point
    | 2 -> Point.getZ point
    | _ -> failwith "Invalid axis provided"

let getVectorAxis axis vector = 
    match axis with
    | 0 -> Vector.getX vector
    | 1 -> Vector.getY vector
    | 2 -> Vector.getZ vector
    | _ -> failwith "Invalid axis provided"


let private findSplitValue axis (boundingBoxes: BoundingBox list) =
    let lowerPoints = List.map fst boundingBoxes
    
    List.averageBy (getPointAxis axis) lowerPoints // Averge of the lower points


let private makeSubtreePair axis splitValue (shape: AbstractShape) =
    let (lower, upper) = shape.GetBoundingBox()
    let (lowerAxis, upperAxis) = getPointAxis axis lower, getPointAxis axis upper

    // Should this shape be appended to left subtree?
    let leftNew = if lowerAxis < splitValue+kdEps then Some(shape) else None
    // Right subtree?
    let rightNew = if upperAxis > splitValue-kdEps then Some(shape) else None

    (leftNew, rightNew)


let private getSurroundingBoundingBox (boundingBoxes: BoundingBox list) =
    // Get all lower and upper points of all shapes
    let minPoints, maxPoints = boundingBoxes |> List.unzip

    // Find the lowest of all minPoints
    let Lx = List.fold (fun acc point -> min (Point.getX point) acc) Double.MaxValue minPoints
    let Ly = List.fold (fun acc point -> min (Point.getY point) acc) Double.MaxValue minPoints
    let Lz = List.fold (fun acc point -> min (Point.getZ point) acc) Double.MaxValue minPoints

    // Find the highest of all maxPoints
    let Hx = List.fold (fun acc point -> max (Point.getX point) acc) Double.MinValue maxPoints
    let Hy = List.fold (fun acc point -> max (Point.getY point) acc) Double.MinValue maxPoints
    let Hz = List.fold (fun acc point -> max (Point.getZ point) acc) Double.MinValue maxPoints
        
    (mkPoint Lx Ly Lz, mkPoint Hx Hy Hz)
                                                                                                            

let private getSplitAxis axis = (axis+1)%3
     
let private order(d, left, right) =
    if d > 0. then
        // If d is positive, keep the order
        (left, right)
    else
        // Otherwise, swap the order
        (right, left)


let rec private search (node : KDTreeNode) (ray : Ray) t t' =
    match node with
    | Leaf shapes -> 
        // Get the closest hit of the Shapes in this Leaf node
        let hit = Shape.GetClosestHit ray shapes
        
        // If there is a hit, ensure that it is within the bounding box of the entire KDTree
        if hit.IsSome && hit.Value.distance < t' then
            hit
        else
            None
    | Node _ -> searchNode node ray t t'
   
and private searchNode (node : KDTreeNode) (R(o,d) as ray) t t' =
    // Pull all the information out of the node
    let (Node((axis, splitValue),left,right)) = node

    // Get the values of the ray along the split axis
    let originAxisValue    = getPointAxis axis o
    let directionAxisValue = getVectorAxis axis d

    // If the direction along the split axis is 0, aka it is perpendicular with the axis
    if directionAxisValue = 0. then
        // Then simply check if the origin point is lower or higher than the splitValue
        if originAxisValue <= splitValue then
            search left ray t t'
        else
            search right ray t t'
    else
        // The difference between the splitValue and origin. Dividing it by the direction extends it, as the direction is normalized
        let tHit = (splitValue - originAxisValue) / directionAxisValue
        // Check if the order should be switched, based on the direciton. This ensures that the Ray hits 'first' first and 'second' thereafter
        let first, second = order(directionAxisValue, left, right)

        // If it can be deduced that the ray ONLY hits the right side, then only search that subtree
        if tHit <= t || tHit <= 0. then
            search second ray t t'
        else
            // Otherwise, if it can be deduced that the ray only hits the left subtree, then only search that
            if tHit >= t' then
                search first ray t t'
            else
                // We must check both subtrees
                // Search the first subtree, then the second. If we hit in the first subtree, then we don't have to check the second, as the hit would be behind a shape in the first subtree
                let searchFirst = search first ray t tHit
                if searchFirst.IsSome then
                    searchFirst
                else
                    search second ray tHit t'

let private traverse (tree : KDTree) (R(o,d) as ray) =
    // Unwrap the details of the tree
    let root, (lower, upper) = tree

    // Check if the ray hits the trees bounding box
    let hitTree = Shape.BBHitFunc o d lower upper

    match hitTree with
    // If the ray hits the bounding box, search and determine if it hits any shape within
    | Some (t, t') -> search root ray t t'
    // Otherwise there is no hit at all
    | None -> None



// ------------------------------------------------ //
// ---------------- Public methods ---------------- //
// ------------------------------------------------ //

let private mkKDTree (inputShapes: AbstractShape list) =
    // Calculate the maximum depth of the tree based on the log2(shapes.Length) - Limit it to 30
    let maxDepth = min (int (Math.Ceiling (log(float inputShapes.Length) / log(2.)))) 30

    printfn "[CONSTRUCT] Generating KD-Tree with %d Shapes, having %d max depth" inputShapes.Length maxDepth

    let rec inner depth (shapes: AbstractShape list) axis =
        // Check if there is enough shapes to make a node
        if shapes.Length <= 8 then
            Leaf(shapes)
        else
            let boundingBoxes = List.map (fun (s: AbstractShape) -> s.GetBoundingBox()) shapes 

            // Determine the splitValue
            let splitValue = findSplitValue axis boundingBoxes

            let shapeDistribution = [|
                for shape in shapes do
                    yield makeSubtreePair axis splitValue shape|] // TODO Could be made parrallel

            // Find duplicates based on the shapeDistribution
            let duplicates = Array.fold (fun acc (left: AbstractShape option, right: AbstractShape option) -> if left.IsSome && right.IsSome then acc+1 else acc) 0 shapeDistribution

            // Unzip the two lists and ignore any None
            let left, right = Array.unzip shapeDistribution
            let leftShapes = Array.choose id left |> List.ofArray
            let rightShapes = Array.choose id right |> List.ofArray

            // Find the split axis
            let nextAxis = getSplitAxis axis

            // If x percentage of shapes are in both lists or if the maxDepth has been reached, make this node a Leaf
            if (float duplicates) / (float shapes.Length) > 0.6 || depth >= maxDepth then
                Leaf(shapes)
            else
                // Create the node, defined by the Split and recursively call on the left- and rightShapes to create the subtrees
                Node((axis, splitValue), inner (depth+1) leftShapes nextAxis, inner (depth+1) rightShapes nextAxis)

    // Start the recursive call with depth 1 and all shapes
    let root = inner 1 inputShapes 0
    let boundingBox = getSurroundingBoundingBox (List.map (fun (s: AbstractShape) -> s.GetBoundingBox()) inputShapes)


    (root, boundingBox)


let private mkTreeShape kdTree =
    // Get the bounding box
    let _, bb = kdTree

    // Define the hitFunc as a function that needs the transNormFunc and a ray, and then traverses the given KDTree 
    let hitFunc transNormFunc (textures: Texture list) ray = traverse kdTree ray

    // Convert into the Tree BaseShape
    new Tree (hitFunc, bb) :> BaseShape

let mkShapeKDTree (shapes: AbstractShape list) =
    mkKDTree shapes |> mkTreeShape
