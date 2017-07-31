module Debug

open Types

let cellToText (colour:colour) : string = 
    match colour with 
    | colour.Colour(0uy,0uy) -> "Light Red"
    | colour.Colour(0uy,1uy) -> "Red"
    | colour.Colour(0uy,2uy) -> "Dark Red"
    | colour.Colour(1uy,0uy) -> "Light Yellow"
    | colour.Colour(1uy,1uy) -> "Yellow"
    | colour.Colour(1uy,2uy) -> "Dark Yellow"
    | colour.Colour(2uy,0uy) -> "Light Green"
    | colour.Colour(2uy,1uy) -> "Green"
    | colour.Colour(2uy,2uy) -> "Dark Green"
    | colour.Colour(3uy,0uy) -> "Light Cyan"
    | colour.Colour(3uy,1uy) -> "Cyan"
    | colour.Colour(3uy,2uy) -> "Dark Cyan"
    | colour.Colour(4uy,0uy) -> "Light Blue"
    | colour.Colour(4uy,1uy) -> "Blue"
    | colour.Colour(4uy,2uy) -> "Dark Blue"
    | colour.Colour(5uy,0uy) -> "Light Magenta"
    | colour.Colour(5uy,1uy) -> "Magenta"
    | colour.Colour(5uy,2uy) -> "Dark Magenta"
    | _ -> failwith("Unknown Colour")


let stackToText (stack:List<int>) = 
    stack |> List.iter (printf "%d ")