module PlyParser

open Point
open Vector
open System.IO

type MeshInfo = {
    vertices: float array array;
    faces: (int*int*int) array;
    nVertices: int;
    nFaces: int;
    vertexProperties: string list;
    faceProperties: string list;
    smooth: bool;
}

val parsePly: string -> bool -> MeshInfo