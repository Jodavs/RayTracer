module Program

open RayTracer
open Shape
open BaseShape
open CSG
open Colour
open Vector
open Point
open Types
open Matrix
open AffineTransform
open System
open ImplicitSurfaces
open PlyParser
open Mesh
open Texture
open KDMeshTree
open KDShapeTree

let camera = {
    position = mkPoint 0. 0. 10.;
    lookat = mkPoint 0. 0.1 0.;
    up = mkVector 0. 1. 0.;
    zoom = 8.;
    width = 10.;
    height = 10.;
    resX = 800;
    resY = 800;}


let renderSettings = [ ("render_blocksize", 128) ] |> Map.ofSeq


let scene : RayTracer.Scene = {
    shapes = [
                Shape(Rectangle(5., 5.), [ColourTexture(RGB(mkColour 0. 0. 1., 0.))], AffineTransform(Matrix.identity, Matrix.identity, false))
    ];
    lights = [{
                 position = mkPoint 0. 0. 20.;
                 colour = mkColour 1. 1. 1.;
                 intensity = 0.5
             }];
    ambientLight = (mkColour 1. 1. 1., 0.05);
    maxReflections = 4
}


[<STAThreadAttribute>]
RayTracer.renderAsync scene camera renderSettings ""