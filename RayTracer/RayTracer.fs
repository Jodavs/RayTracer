module RayTracer

open System
open System.Drawing
open System.Drawing.Drawing2D
open System.Windows.Forms
open System.Windows.Forms.Design
open Colour
open Vector
open Point
open AffineTransform
open Types
open Matrix
open Shape
open KDShapeTree
open System.Threading.Tasks
open System.Threading


type Scene = {shapes: AbstractShape list; lights: Light list; ambientLight: AmbientLight; maxReflections: int}

let renderPixel (scene:Scene) cameraRay (x:int) (y:int) =
    let ray = cameraRay (float x) (float y)

    let ambientLightColour, ambientLightIntensity = scene.ambientLight // TODO Should we store it as a record instead to avoid this?

    let ambient = (ambientLightIntensity*ambientLightColour)

    match Shape.GetClosestHit ray scene.shapes with
    | None   -> Color.Black
    | Some h -> 
        let rec inner hit (R(ori, dir)) (curColour: Colour) numReflects =       
            let hitPoint = ori + hit.distance*dir
            let baseColour, reflec = match hit.material with RGB (mat,ref) -> (mat,ref)

            let norm = if Vector.dotProduct dir hit.normal > 0. then -hit.normal else hit.normal

            //let norm = -hit.normal // TODO PROBLEM 1 (Some normals are flipped)

            let lightContribution = 
                // Loop over all lights, calculating the colour they contribute (if any) and add it to the accumulator
                List.fold (fun acc (light: Light) -> 
                    let R(_, sd) as shadowRay = Shape.MakeShadowRay hitPoint norm light.position (Shape.eps) // TODO PROBLEM 2 (Should it be eps?)



                    // Angle between norm and shadowRay (Determines the intensity of the light)
                    let c1 = Vector.dotProduct norm sd

                    let lightDistance = Point.distance hitPoint light.position |> Vector.magnitude

                    // If the angle is positive (hit on the visible side) and no shapes inbetween the origin and light, then there is an unobsured line of sight to the light
                    if c1 > 0. then
                        
                        let hitAnyShape = Shape.GetClosestHit shadowRay scene.shapes // TODO We should stop if we hit one with lower distance, instead of keep checking
                        match hitAnyShape with
                        | Some hit ->
                            
                            if lightDistance < hit.distance then (c1*light.intensity*light.colour) + acc else acc
                        | None -> (c1*light.intensity*light.colour) + acc
                    else
                        acc
                    ) curColour scene.lights

            // Calculate the r,g,b that this material would have (baseColour multiplied the light contribution)
            let ourColour = baseColour * lightContribution

            // The reflection calculations
            // if this material is reflective and has not already bounced more than the limit
            if reflec > 0. && numReflects < scene.maxReflections then
                let movedHitPoint = hitPoint + (Shape.eps * norm)

                // Calculate the reflection direction. It is based on the angle between the norm and original direction
                let newDir = (dir - (2.*(Vector.dotProduct norm dir)) * norm) |> Vector.normalise
                let newRay = R(movedHitPoint, newDir)

                // Calculate if the reflection ray hits any shape
                let newHit = Shape.GetClosestHit newRay scene.shapes

                // If it does, intermix this colour with the reflection colour
                if newHit.IsSome then
                    let reflectiveColour = inner newHit.Value newRay (mkColour 0. 0. 0.) (numReflects+1)
                    Colour.merge reflec ourColour reflectiveColour
                else
                    ourColour
            else
                ourColour

        // The start color is the ambient
        Colour.toColor (inner h ray (ambient) 0)


/// Builds a sequence of blocks based on the given scenes render_blocksize
let renderBlockSeq cam (renderSettings:Map<String, int>) =
    let blockSize = renderSettings.Item "render_blocksize"
    seq { for y in 0..(cam.resY/blockSize) do
                for x in 0..(cam.resX/blockSize) do
                    // Don't return blocks beyond the borders of the image
                    let endX = if cam.resX - (x+1)*blockSize <= 0 then cam.resX-1 else (x+1)*blockSize
                    let endY = if cam.resY - (y+1)*blockSize <= 0 then cam.resY-1 else (y+1)*blockSize
                     
                    // Yield the result in a (x,y) list format
                    yield [ for y in [y*blockSize..endY] do
                                for x in [x*blockSize..endX] do
                                    yield (x,y) ]
                     
    }

    
let resizeImage (image: Bitmap, newWidth: int, newHeight: int) =
    let resizedImage = new Bitmap(newWidth, newHeight)
    let g = Graphics.FromImage(resizedImage)
    g.InterpolationMode <- InterpolationMode.HighQualityBicubic
    g.DrawImage(image, 0, 0, newWidth, newHeight)
    resizedImage
        
/// Given a scene and a image, asynchronously draw the scene on the image in blocks
let renderAsync (inputScene: Scene) camera (renderSettings : Map<String,int>) (path: string) =
    let image = new Bitmap(camera.resX, camera.resY)
    let tree = new Shape(mkShapeKDTree inputScene.shapes, [], AffineTransform.identity)
    let scene = {shapes = [tree]; lights = inputScene.lights; maxReflections = inputScene.maxReflections; ambientLight = inputScene.ambientLight} 
    let pixelWidth = camera.width/(float camera.resX)
    let pixelHeight = camera.height/(float camera.resY)

    let pixelCenterX x = pixelWidth * (x - (float camera.resX)/2. + 0.5)
    let pixelCenterY y = pixelHeight * (y - (float camera.resY)/2. + 0.5)

    let time = System.DateTime.Now

    let orthFrame cam : OrthFrame =
        let w = Point.direction cam.position cam.lookat
        let u = cam.up % w
        let v = w % u
        (v, u, w)

    let cameraRay x y =
        let (v, u, w) = orthFrame camera
        R(camera.position, ((pixelCenterX x)*(-u) + (pixelCenterY y)*(-v) + camera.zoom*w) |> Vector.normalise) // TODO We broke the rules.. alot

    let renderAsyncToFile () =
        let accessMutex = new Mutex()
        (Async.Parallel [
                for block in (renderBlockSeq camera renderSettings) -> async {
                    // For each block, render all the pixels within
                    let blockResult = List.map (fun (x, y) -> (x, y, renderPixel scene cameraRay x y)) block
                    // And then make the GUI thread update the image with the result
                    accessMutex.WaitOne() |> ignore
                    List.iter (fun (x, y, color) -> image.SetPixel(x, y, color)) blockResult ; printfn "%A" (System.DateTime.Now - time)
                    accessMutex.ReleaseMutex() |> ignore
                }
            ] |> Async.StartAsTask).Wait() |> ignore
        image.Save(path) 


    /// Given a scene and a image, asynchronously draw the scene on the image in blocks
    let renderAsyncToScreen () =
        let form = new Form()
        let pictureBox = new PictureBox(Dock = DockStyle.Fill)
        let saveLb = new Label()
        let saveMenu = new SaveFileDialog()

        saveLb.Size <- new Size(64, 64)
        saveLb.Hide()
        saveLb.BackColor <- Color.Transparent
        saveLb.Image <- Image.FromFile(@"../../../images/floppy.png")
        
        saveMenu.Filter <- "Image files (*.jpg)|*.jpg|All files (*.*)|*.*"

        form.Controls.Add(saveLb)
        form.Controls.Add(pictureBox)

        saveLb.Parent <- pictureBox

        pictureBox.MouseClick.Add (fun ev ->          
            let mutable pos = ev.Location
            pos.Offset(-25, -25)
            if saveLb.Visible then saveLb.Hide() else 
            saveLb.Show()
            saveLb.Location <- pos)

        let loadingImage = new Bitmap(Image.FromFile("../../../images/loading.png"))
        let image = resizeImage(loadingImage, camera.resX, camera.resY)
        pictureBox.Image <- image
        pictureBox.SizeMode <- PictureBoxSizeMode.StretchImage
        form.ClientSize <- Size(camera.resX, camera.resY)
        form.StartPosition <- FormStartPosition.CenterScreen

        saveLb.Click.Add (fun _ -> 
            saveLb.Hide()
            saveMenu.FileName <- (time.ToString().Replace(":","-") + ".jpg")
            if saveMenu.ShowDialog() = DialogResult.OK then (image.Save(saveMenu.FileName)))

        let time = System.DateTime.Now

        let renderImageDelegate (image:Bitmap) = 
            new RenderImageDelegate (fun blockResult -> List.iter (fun (x, y, color) -> image.SetPixel(x, y, color)) blockResult ; printfn "%A" (System.DateTime.Now - time) ; pictureBox.Refresh())
        
        Async.Parallel [
                for block in (renderBlockSeq camera renderSettings) -> async {
                    // For each block, render all the pixels within
                    let blockResult = List.map (fun (x, y) -> (x, y, renderPixel scene cameraRay x y)) block
                    // And then make the GUI thread update the image with the result
                    pictureBox.BeginInvoke (renderImageDelegate image, blockResult) |> ignore
                }
            ] |> Async.StartAsTask |> ignore

        Application.Run(form)

    if (path = "") then renderAsyncToScreen () // Render to file if a path is given
    else renderAsyncToFile () // Render to screen otherwise