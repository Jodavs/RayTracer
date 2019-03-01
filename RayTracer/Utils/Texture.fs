module Texture

open Colour
open Types

open System.Drawing
open System.Threading

[<AbstractClass>]
type Texture() =
    abstract member GetMaterial: float -> float -> Material

type ColourTexture(color: Material) =
    inherit Texture()
    override ct.GetMaterial _ _ = color

type ImageTexture(path: string, reflectivity: float) =
    inherit Texture()
    let img = new Bitmap(path)
    let width = float img.Width
    let height = float img.Height

    let accessMutex = new Mutex()

    override it.GetMaterial x y = 
        accessMutex.WaitOne() |> ignore
        let mat = RGB (img.GetPixel(int(x*width), int(height - y*height)) |> Colour.fromColor, reflectivity)
        accessMutex.ReleaseMutex() |> ignore
        mat

type TextureFunction(f: (float -> float -> Material)) =
    inherit Texture()
    override tf.GetMaterial x y = f x y