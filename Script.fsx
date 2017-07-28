#load "Library1.fs"
open Piet.net
open System.Drawing

type hue = byte  //0..5
type lightness = byte  //0..2

type colour = 
    | White
    | Black
    | Colour of hue * lightness

type program  = colour[,]

type dp =
    | Right
    | Down
    | Left
    | Up

type cc =
    | Left
    | Right

type command =
    | Push
    | Pop
    | Add
    | Substract
    | Multiple
    | Divide
    | Mod
    | Not
    | Greater
    | Pointer
    | Switch
    | Duplicate
    | Roll
    | InNumber
    | InChar
    | OutNumber
    | OutChar


let colourToCell (color:Color) : colour = 
    match int(color.R), int(color.G), int(color.B) with
    | 0xFF, 0xC0, 0xC0 -> colour.Colour(0uy,0uy)  //Light Red
    | 0xFF, 0x00, 0x00 -> colour.Colour(0uy,1uy)  //Red
    | 0xC0, 0x00, 0x00 -> colour.Colour(0uy,2uy)  //Dark Red

    | 0xFF, 0xFF, 0xC0 -> colour.Colour(1uy,0uy)  //Light Yellow
    | 0xFF, 0xFF, 0x00 -> colour.Colour(1uy,1uy)  //Yellow
    | 0xC0, 0xC0, 0x00 -> colour.Colour(1uy,2uy)  //Dark Yellow

    | 0xC0, 0xFF, 0xC0 -> colour.Colour(2uy,0uy)  //Light Green
    | 0x00, 0xFF, 0x00 -> colour.Colour(2uy,1uy)  //Green
    | 0x00, 0xC0, 0x00 -> colour.Colour(2uy,2uy)  //Dark Green

    | 0xC0, 0xFF, 0xFF -> colour.Colour(3uy,0uy)  //Light Cyan
    | 0x00, 0xFF, 0xFF -> colour.Colour(3uy,1uy)  //Cyan
    | 0x00, 0xC0, 0xC0 -> colour.Colour(3uy,2uy)  //Dark Cyan

    | 0xC0, 0xC0, 0xFF -> colour.Colour(4uy,0uy)  //Light Blue
    | 0x00, 0x00, 0xFF -> colour.Colour(4uy,1uy)  //Blue
    | 0x00, 0x00, 0xC0 -> colour.Colour(4uy,2uy)  //Dark Blue

    | 0xFF, 0xC0, 0xFF -> colour.Colour(5uy,0uy)  //Light Magenta
    | 0xFF, 0x00, 0xFF -> colour.Colour(5uy,1uy)  //Magenta
    | 0xC0, 0x00, 0xC0 -> colour.Colour(5uy,2uy)  //Dark Magenta

    | 0x00, 0x00, 0x00 -> colour.Black

    | _ -> colour.White

let loadImage (image:Bitmap) (codelSize:int) : (int * int * program) = 

    let width = image.Width / codelSize
    let height = image.Height / codelSize
    let a = Array2D.init width height (fun _ _ -> colour.White)

    for x in 0..width-1 do
        for y in 0..height-1 do
            a.[x,y] <- image.GetPixel(x * codelSize, y * codelSize) |> colourToCell

    width, height, a



let getMatchingNeighbours x y width height (program:program) =
    let colourToMatch = program.[x,y]
    seq {
            if x > 0 && colourToMatch = program.[x - 1,y] then yield ( x- 1, y)
            if (x + 1) < width && colourToMatch = program.[x + 1,y] then yield (x + 1, y)
            if y > 0 && colourToMatch = program.[x,y - 1] then yield (x, y - 1)
            if (y + 1) < width && colourToMatch = program.[x,y + 1] then yield (x, y + 1)
        }


let countPixelsInBlock program x y width height : int =
    
    let rec findAdjBlocks toExamine (examined:List<int*int>) =
        match toExamine with
        | [] -> examined
        | (x,y)::xs -> let possibleOptions = getMatchingNeighbours x y width height program
                       
                       // Make sure we haven't already examined them
                       let possibleOptions' = possibleOptions |> Seq.where(fun (x, y) -> not(examined |> List.exists(fun (x', y') -> x' = x && y' = y)))
                       
                       // Make sure we don't already have them in our list to examine
                       let possibleOptions'' = possibleOptions' 
                                                        |> Seq.where(fun (x, y) -> not(toExamine |> List.exists(fun (x', y') -> x' = x && y' = y)))
                                                        |> Seq.toList

                       // Add to the list
                       let toExamine' = xs |> List.append possibleOptions''

                       findAdjBlocks toExamine' ((x,y) :: examined)
        
    let toExamine = [x,y]
    let examined = findAdjBlocks toExamine []
    examined |> Seq.length

let getNextCommand x y (dp:dp) (cc:cc) program : (command * dp * cc * int * int) = 
    []



let image = new Bitmap("C:\Code\piet\Piet.net\Piet_hello_big.png")
let width, height, program = loadImage image 5
countPixelsInBlock program 0 0 width height 
()