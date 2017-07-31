module Types

type hue = byte  //0..5
type lightness = byte  //0..2

type colour = 
    | White
    | Black
    | Colour of hue * lightness

type program  = colour[,]

type direction =
    | Right
    | Down
    | Left
    | Up

type dp = direction

type cc =
    | Left
    | Right

type command =
    | NOP
    | Push
    | Pop
    | Add
    | Substract
    | Multiply
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