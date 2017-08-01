module Parser

open System.Drawing
open Types

let modulo n m = ((n % m) + m) % m


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
            if (y + 1) < height && colourToMatch = program.[x,y + 1] then yield (x, y + 1)
        }


let rec findAdjBlocks program width height toExamine (examined:List<int*int>) =
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

                   findAdjBlocks program width height toExamine' ((x,y) :: examined)


let countPixelsInBlock program x y width height : int =
    let examined = findAdjBlocks program width height [x,y] []
    examined |> Seq.length

let furthestInDirection program x y width height (dp:dp) : (int*int) =
    let examined = findAdjBlocks program width height [x,y] []
    
    match dp with
    | dp.Right -> examined |> List.maxBy(fun (x,y) -> x)  
    | dp.Left -> examined |> List.minBy(fun (x,y) -> x)  
    | dp.Up -> examined |> List.minBy(fun (x,y) -> y)  
    | dp.Down -> examined |> List.maxBy(fun (x,y) -> y)  
    
let rec findEdgeOfBlock width height (program:program) (direction:direction) x y   = 
    //Keep walking until we hit an edge, or a block of a different colour
    let colourToMatch = program.[x,y]
    let next = findEdgeOfBlock width height program direction

    match direction with
    | direction.Left ->  if (x = 0 || program.[x - 1,y] <> colourToMatch) then x,y else next (x - 1) y  
    | direction.Right ->  if (x = (width - 1) || program.[x + 1,y] <> colourToMatch) then  x,y else next (x + 1) y  
    | direction.Up ->  if (y = 0 || program.[x,y - 1] <> colourToMatch) then x,y else next x (y - 1)  
    | direction.Down ->  if (y = (height - 1) || program.[x,y + 1] <> colourToMatch) then x,y else next x (y + 1)  

let moveIntoNextBlock width height (program:program) (dp:dp) startX startY   = 
    let rec next x y = 
        if program.[startX,startY] <> program.[x,y] then
            //We have reached a new block
            Some (x, y)
        else
            match dp with
            | dp.Left ->  if (x = 0) then None else next (x - 1) y  
            | dp.Right ->  if (x = (width - 1)) then None else next (x + 1) y  
            | dp.Up ->  if (y = 0) then None else next x (y - 1)  
            | dp.Down ->  if (y = (height - 1)) then None else next x (y + 1)  

    //Keep walking until we hit an edge, or a block of a different colour
    next startX startY
    

    
let directionFromDPandCC (dp:dp) (cc:cc) = 
    match dp, cc with
    | dp.Right, cc.Left -> direction.Up
    | dp.Right, cc.Right -> direction.Down
    | dp.Down, cc.Left -> direction.Right
    | dp.Down, cc.Right -> direction.Left
    | dp.Left, cc.Left -> direction.Down
    | dp.Left, cc.Right -> direction.Up
    | dp.Up, cc.Left -> direction.Left
    | dp.Up, cc.Right -> direction.Right


let toggle = function 
            | cc.Left -> Right
            | cc.Right -> Left

let moveDPClockwise = function
                    | dp.Left -> dp.Up
                    | dp.Up -> dp.Right
                    | dp.Right -> dp.Down
                    | dp.Down -> dp.Left

let moveDPAntiClockwise = function
                        | dp.Left -> dp.Down
                        | dp.Down -> dp.Right
                        | dp.Right -> dp.Up
                        | dp.Up -> dp.Left

let getNextCommand program width height x y (dp:dp) (cc:cc) = 
    
    let rec worker (dp:dp) (cc':cc) attempt = 

        let x', y' = furthestInDirection program x y width height dp
        let dir = directionFromDPandCC dp cc'
        let x'', y'' = findEdgeOfBlock width height program dir x' y'   
        let next = moveIntoNextBlock width height program dp x'' y''

        let isOk,inWhite =  match next with
                            | None -> false, false //We have hit an edge
                            | Some(x',y') when program.[x', y'] = colour.Black -> false, false //We have hit a restriction
                            | Some(x',y') when program.[x', y'] = colour.White -> false, true  //We have entered a white block
                            | _ -> true, false //We are ok.

        if inWhite then
            // Keep going until we hit something
            let x,y = next.Value
            let next' = moveIntoNextBlock width height program dp x y
            Some(next'.Value, dp, cc', false)
        else
            if (isOk) then
                Some(next.Value, dp, cc', true)
            else
                match attempt with
                | 0 | 2 | 4 | 6-> worker dp (toggle cc') (attempt + 1)
                | 1 | 3 | 5 | 7-> worker (moveDPClockwise dp) cc' (attempt + 1)
                | _ -> None

    worker dp cc 0

let decodeCommand (fromColour:colour) (toColour:colour) : command =
    let (h1,l1) = match fromColour with
                  | Colour(h,l) -> (h,l)
                  | _ -> failwith("FromColour Must be a colour")

    let (h2,l2) = match toColour with
                  | Colour(h,l) -> (h,l)
                  | _ -> failwith("ToColour Must be a colour")

    let hueChange = if h2 >= h1 then 
                        int(h2) - int(h1)
                    else
                        (6 - int(h1)) + int(h2)


    let lightnessChange = modulo (int(l2) - int(l1)) 3

    match hueChange, lightnessChange with
    | 0,0 -> NOP
    | 0,1 -> Push
    | 0,2 -> Pop
    | 1,0 -> Add
    | 1,1 -> Substract
    | 1,2 -> Multiply
    | 2,0 -> Divide
    | 2,1 -> Mod
    | 2,2 -> Not
    | 3,0 -> Greater
    | 3,1 -> Pointer
    | 3,2 -> Switch
    | 4,0 -> Duplicate
    | 4,1 -> Roll
    | 4,2 -> InNumber
    | 5,0 -> InChar
    | 5,1 -> OutNumber
    | 5,2 -> OutChar
    | _,_ -> failwith("Unexpected hue/lightness change")