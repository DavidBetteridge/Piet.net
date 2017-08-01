module Debug

open Types

let cellToText (colour:codel) : string = 
    match colour with 
    | codel.Colour(hue.Red,lightness.Light)  -> "Light Red"
    | codel.Colour(hue.Red,lightness.Normal) -> "Red"
    | codel.Colour(hue.Red,lightness.Dark)   -> "Dark Red"
    | codel.Colour(hue.Yellow,lightness.Light)  -> "Light Yellow"
    | codel.Colour(hue.Yellow,lightness.Normal) -> "Yellow"
    | codel.Colour(hue.Yellow,lightness.Dark)   -> "Dark Yellow"
    | codel.Colour(hue.Green,lightness.Light)  -> "Light Green"
    | codel.Colour(hue.Green,lightness.Normal) -> "Green"
    | codel.Colour(hue.Green,lightness.Dark)   -> "Dark Green"
    | codel.Colour(hue.Cyan,lightness.Light)  -> "Light Cyan"
    | codel.Colour(hue.Cyan,lightness.Normal) -> "Cyan"
    | codel.Colour(hue.Cyan,lightness.Dark)   -> "Dark Cyan"
    | codel.Colour(hue.Blue,lightness.Light)  -> "Light Blue"
    | codel.Colour(hue.Blue,lightness.Normal) -> "Blue"
    | codel.Colour(hue.Blue,lightness.Dark)   -> "Dark Blue"
    | codel.Colour(hue.Magenta,lightness.Light)  -> "Light Magenta"
    | codel.Colour(hue.Magenta,lightness.Normal) -> "Magenta"
    | codel.Colour(hue.Magenta,lightness.Dark)   -> "Dark Magenta"
    | _ -> failwith("Unknown Colour")


let stackToText (stack:List<int>) = 
    stack |> List.iter (printf "%d ")