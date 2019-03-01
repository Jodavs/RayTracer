module PlyParser

open System
open System.IO
open Vector
open Point

type Vertex = float array
type Face = int*int*int

exception PlyParserException of string*int

// This is the actual byte contents of the string "end_header". Used for detecting the end
// position of the header with a BinaryReader
let headerEnd: byte[] = [| 101uy;110uy;100uy;95uy;104uy;101uy;97uy;100uy;101uy;114uy; |]

// Finds the starting location of the data (after the header)
let getStart (reader:BinaryReader) =
    let windowSize = headerEnd.Length
    let buffer = reader.ReadBytes(10000)

    let mutable i = 0
    let mutable result = None
    let mutable shouldReturn = false
    // Move window by 1 until "end_header" is found
    while not shouldReturn && i < (buffer.Length-windowSize) do
        let window = Array.sub(buffer) i windowSize
        if window = headerEnd then
            i <- i+windowSize
            // Skip past extra whitespace after "end_header"
            while buffer.[i] = 13uy || buffer.[i] = 10uy do i <- i+1
            result <- Some(i)
            shouldReturn <- true
        i <- i+1
    // Return the start position of actual data
    result

// Helper function for reading a line of text and splitting it by each space
let getNextLine (file:StreamReader) = file.ReadLine().Split(' ') |> List.ofArray

// Takes a type string (as defined by a ply header) and returns an appropiate
// function for reading a binary value of that type and converting it to a floating point number
let getReadFloatFromType s (bigEndian:bool) (b:BinaryReader): (unit -> float) =
    // Used to wrap returned function so we don't actually run it immediatly
    let inline wrap f = fun _ -> f 
    //let rev = if not bigEndian then Array.rev else id
    let readBytes n = b.ReadBytes(n) |> Array.rev // For some reason we always need to reverse...
   
    // Simply match the type string and create the returned function
    match s with
    | "char"    | "int8"    -> b.ReadSByte() |> float |> wrap                           
    | "uchar"   | "uint8"   -> b.ReadByte() |> float |> wrap   
    | "short"   | "int16"   -> BitConverter.ToInt16(readBytes 2, 0) |> float |> wrap
    | "ushort"  | "uint16"  -> BitConverter.ToUInt16(readBytes 2, 0) |> float |> wrap
    | "int"     | "int32"   -> BitConverter.ToInt32(readBytes 4, 0) |> float |> wrap
    | "uint"    | "uint32"  -> BitConverter.ToUInt32(readBytes 4, 0) |> float |> wrap
    | "float"   | "float32" -> BitConverter.ToSingle(readBytes 4, 0) |> float |> wrap
    | "double"  | "float64" -> BitConverter.ToDouble(readBytes 8, 0) |> float |> wrap
    | _ -> raise (PlyParserException ("Invalid property format in ply file", 0))

type PlyHeader = {
    binary: bool;
    bigEndian: bool;
    nVertices: int;
    nFaces: int;
    vertexProperties: ((BinaryReader -> (unit -> float))*string) list;
    faceProperties: string list
}

let emptyHeader = {
    binary=false;
    bigEndian=false;
    nVertices=0;
    nFaces=0;
    vertexProperties=[];
    faceProperties=[]
}

// Simple helpers for parsing strings to number types
let parseInt = System.Int32.Parse
let parseFloat f = System.Double.Parse(f, System.Globalization.CultureInfo.InvariantCulture)

// Function for matching any ply header. Saves the info into a PlyHeader record.
let rec matchHeaderIt file acc = 
    match getNextLine file with
    | "format"::f::[_]         -> match f with // Sets appropiate values based on format definition
                                  | "ascii" -> {acc with binary=false}
                                  | "binary_big_endian" -> {acc with binary=true; bigEndian=true}
                                  | "binary_little_endian" -> {acc with binary=true; bigEndian=false}
                                  | _ -> raise (PlyParserException ("Invalid file format string in ply file", 0))
                                  |> matchHeaderIt file
    | "element"::"vertex"::[n] -> {acc with nVertices=parseInt n} 
                                    |> matchHeaderIt file
    | "element"::"face"::[n]   -> {acc with nFaces=parseInt n} 
                                    |> matchHeaderIt file
    // Takes the type string from the header and converts it into a funtion (see getReadFloatFormType)
    | "property"::t::[name]    -> {acc with vertexProperties=[(getReadFloatFromType t acc.bigEndian, name)]@acc.vertexProperties} 
                                    |> matchHeaderIt file
    | "property"::"list"::_::_::[name] 
                               -> {acc with faceProperties=[name]@acc.faceProperties}
                                    |> matchHeaderIt file
    | "end_header"::_          -> acc // Return result when end of header is reached
    | _                        -> matchHeaderIt file acc

// Get header information from ply file. Returns a PlyHeader record, which contains information on the number of vertices and
// faces, and a list of properties of each vertex and face.
let rec matchHeader file = matchHeaderIt file emptyHeader

// Text parsing functions

// Parse a list of strings representing a vertex
let parseVertex = List.map parseFloat >> Array.ofList

// Since each face contains exactly four values, simply parse the last three and return the resulting Face
let parseFace = function
    | _::a::b::[c] -> (parseInt a, parseInt b, parseInt c)
    | _ -> raise (PlyParserException ("malformed face line (should be 3, a, b, c)", 0))
    
// Parse n lines of vertices
let rec parseNLines parser file acc = function
    | 0 -> acc
    | n ->
        // This parses the next line in the file with the parser supplied with the function.
        // The filter is used to remove unwanted whitespace
        let a = getNextLine file |> List.filter (fun e -> e <> "") |> parser
        parseNLines parser file (a::acc) (n-1)
     
// Helpers created to make the parseNLines function easy to use
let parseVertices file header = parseNLines parseVertex file [] header.nVertices
let parseFaces file header = parseNLines parseFace file [] header.nFaces

// Binary parsing functions

// l: a list of (BinaryReader -> unit -> float)*string, which represents the vertex properties of the given ply file
// b: the binary reader to read from
// e: the (BinaryReader -> unit -> float) part of each property. We simply apply the function with the 
//    supplied binary reader (b) and unit parameter ().
// The whole List.map is wrapped in a function 
let parseBinaryVertex (b:BinaryReader) vertexProperties = (fun () -> List.map (fun (converter, _) -> converter b ()) vertexProperties |> Array.ofList)

// simple parser for binary faces. Easy, since every single face consists of four int32 values.
let parseBinaryFace (b:BinaryReader) header = (fun () ->
    let _ = b.ReadByte() //|> printfn "%A"
    // If we use endian-ness different from the system, we need to reverse the array. Otherwise, simply return the id function
    let rev = if header.bigEndian then Array.rev else id
    let read = fun () -> BitConverter.ToInt32(b.ReadBytes(4) |> rev, 0)
    (read(), read(), read())
)

// Parse n elements using the supplied parser function
let parseNBinaryElements header parser =
    //let parser = parseBinaryVertex reader header.vertexProperties
    let rec parseHelper acc = function
        | 0 -> acc
        | n -> parseHelper (parser()::acc) (n-1)
    parseHelper [] (header.nVertices)

// Simple helpers for parsing n vertices or faces
let parseNBinaryVertices reader header = parseNBinaryElements header (parseBinaryVertex reader header.vertexProperties)
let parseNBinaryFaces reader header = parseNBinaryElements header (parseBinaryFace reader header)

type MeshInfo = {
    vertices: Vertex array;
    faces: (int*int*int) array;
    nVertices: int;
    nFaces: int;
    vertexProperties: string list;
    faceProperties: string list;
    smooth: bool;
}

// Only function in this module visible to the outside world. Parses any valid ply file (binary or text) and returns
// a tuple of a list of vertices and a list of faces.
let parsePly path smooth =
    let file = File.OpenText(path)
    // Check that we actually opened a ply file
    if file.ReadLine() <> "ply" then raise (PlyParserException ("invalid file format", 0) )
    let header = matchHeader file // Then get the header info
  
    let (vertices, faces) =
        if not header.binary // Parse either a text or binary ply file
        then (parseVertices file header, parseFaces file header)
        else
            let reader = new BinaryReader(File.OpenRead(path))
            match getStart (new BinaryReader(File.OpenRead(path))) with // get starting position of data
            | None -> raise (PlyParserException ("invalid header (no end_header element)", 0))
            | Some(n) -> let _ = reader.ReadBytes(n)
                         (parseNBinaryVertices reader header, parseNBinaryFaces reader header)

    {
        vertices=vertices |> List.rev |> Array.ofList;
        faces=faces |> List.rev |> Array.ofList;
        nVertices=header.nVertices;
        nFaces=header.nFaces;
        vertexProperties=(List.map snd header.vertexProperties);
        faceProperties=header.faceProperties;
        smooth=smooth;
    }