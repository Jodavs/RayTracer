module Mesh

open System

open Vector
open Point
open Colour
open Types
open Matrix
open AffineTransform
open Shape
open PlyParser
open BaseShape

// Convert the float arrays to points
let getVertices meshInfo =
    Array.map (fun (vertex:float array) -> mkPoint vertex.[0] vertex.[1] vertex.[2]) meshInfo.vertices

// Takes an array of points (vertices) and a triple with the corresponding faces
// Return a triple of points, refering to a triangle
let getVerticesFromFace (vertices: Point array) (faceA, faceB, faceC) =
    (vertices.[faceA], 
     vertices.[faceB], 
     vertices.[faceC])

//
let genFaceNormals vertices faces =
    // 
    let faceNormal face =
        // a, b and c representing a 

        let a, b, c = getVerticesFromFace vertices face

        let u = b - a
        let v = c - a

        // Calculate the normal of a face
        let cross = u % v

        // Normalise the vector
        Vector.normalise cross

    // Calculate the normals of all the faces
    Array.map faceNormal faces

//
let genBelongsArray vertices faces =
    // Create an array that contains list of integers with the length corresponding to the amount of vertices
    let (arr: int list array) = Array.create(Array.length vertices) []

    // Append the faceId to the vertexId.
    let append faceIdx vertexIdx = arr.[vertexIdx] <- faceIdx::arr.[vertexIdx]

    // Iterate over faces. Get the triple of the face and append the corresponding faceId to each of them.
    Array.iteri (fun i face -> let va, vb, vc = face
                               append i va
                               append i vb
                               append i vc) faces

    // Return the array appended to above 
    arr


let genVertexNormals vertices faces (faceNormals: Vector array) =
    let belongsArray = genBelongsArray vertices faces

    let rec calcNormSum acc = function
    | [] -> acc
    | faceIdx::xs -> calcNormSum (acc+faceNormals.[faceIdx]) xs

    Array.map (fun faceList -> calcNormSum (mkVector 0. 0. 0.) faceList |>  Vector.normalise  ) belongsArray

let getAllVertices meshInfo = getVertices meshInfo
let getFaces meshInfo = meshInfo.faces
let getFaceNormals vertices faces = genFaceNormals vertices faces
let getVertexNormals vertices faces faceNormals = genVertexNormals vertices faces faceNormals

let retmin f point vertex = 
    let mx, my, mz = Point.getCoord point
    let x, y, z = Point.getCoord vertex
    mkPoint (f mx x) (f my y) (f mz z)

let getUVCoordinates (vertexArray: float array array) (vertexProperties:string list) =
    let extractUV (vertex:float array) =
        match List.tryFind(fun v -> v = "u" || v = "v") vertexProperties with
        | Some _ -> (vertex.[7], vertex.[6])
        | None -> (0., 0.)
    Array.map extractUV vertexArray

let getTriangles vertices (vertexNormals: Vector array) faces faceNormals smooth (uvCoords: (float*float) array) = 
    let genTriangle face normal =
        let a, b, c = getVerticesFromFace vertices face
        let facea, faceb, facec = face
        let norma = vertexNormals.[facea]
        let normb = vertexNormals.[faceb]
        let normc = vertexNormals.[facec]
        let faceNormal = normal

        let ua, va = uvCoords.[facea]
        let ub, vb = uvCoords.[faceb]
        let uc, vc = uvCoords.[facec]   

        new Triangle(a, b, c, norma, normb, normc, faceNormal, smooth, [ua; ub; uc], [va; vb; vc])
    Array.map2 (genTriangle) faces faceNormals

let genTriangleList meshInfo =
    let vertices = getAllVertices meshInfo
    let faces = getFaces meshInfo
    let faceNormals = getFaceNormals vertices faces
    let vertexNormals = getVertexNormals vertices faces faceNormals
    let uvCoords = getUVCoordinates meshInfo.vertices meshInfo.vertexProperties
    List.ofArray (getTriangles vertices vertexNormals faces faceNormals meshInfo.smooth uvCoords)