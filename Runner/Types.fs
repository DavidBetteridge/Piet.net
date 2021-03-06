﻿module Types

type hue = 
    | Red = 0
    | Yellow = 1
    | Green = 2
    | Cyan = 3
    | Blue = 4
    | Magenta = 5

type lightness = 
    | Light = 0
    | Normal = 1
    | Dark = 2

type codel = 
    | White
    | Black
    | Colour of hue * lightness

type program  = codel[,]

type command =
    | NOP
    | Push
    | Pop
    | Add
    | Subtract
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

type direction =
    | Right
    | Down
    | Left
    | Up

type dp = direction

type cc =
    | Left
    | Right



let p3 = codel.Black
let a = 
    match p3 with
    | White -> "White"
    | Colour(h,l) -> "A Colour"
