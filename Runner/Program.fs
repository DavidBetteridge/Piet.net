﻿open System.Drawing
open Types
open Parser


let rotateStack (stack:List<int>) (depth:int) =
    let first = stack.Head
    let popped = stack.GetSlice (Some(1), Some(depth - 1))
    let rest = stack.GetSlice (Some(depth), None)
    let withTop = first :: rest
    withTop|> List.append popped 

let rec rollStack (stack:List<int>) (depth:int) (timesToRoll:int) =
    match timesToRoll with
    | 0 -> stack
    | _ -> rollStack (rotateStack stack depth) depth (timesToRoll - 1)

    
let doCommand previousNumber cmd stack (cc:cc) (dp:dp) = 
    match cmd with
    | command.Push -> //Add the previous number onto the stack
                      previousNumber :: stack, cc, dp

    | command.Not -> match stack with
                     | [] -> failwith("Stack is empty")
                     | x::xs when x = 0 -> 1::xs, cc, dp 
                     | x::xs -> 0::xs, cc, dp

    | command.Pop -> match stack with
                     | [] -> failwith("Stack is empty")
                     | x::xs -> xs, cc, dp 

    | command.OutChar -> //Pop the value from the top of the stack and print it.
                      match stack with
                      | [] -> failwith("Stack is empty")
                      | x::xs -> printf "%A" (x |> char)
                                 xs, cc, dp

    | command.Duplicate ->  match stack with
                            | [] -> failwith("Stack is empty")
                            | x::xs -> (x::stack), cc, dp 
    
    | command.Mod -> match stack with
                     | x::y::xs -> let a = modulo y x
                                   (a::xs), cc, dp 
                     | _ -> failwith("Stack is empty")
                     
    | command.Add -> match stack with
                     | n::m::xs -> let s = n + m
                                   s :: xs, cc, dp 
                     | _ -> failwith("Stack is empty")

    | command.Substract -> match stack with
                           | n::m::xs -> let s = m - n
                                         s :: xs, cc, dp 
                           | _ -> failwith("Stack is empty")

    | command.Multiply -> match stack with
                           | n::m::xs -> let s = m * n
                                         s :: xs, cc, dp 
                           | _ -> failwith("Stack is empty")

    | command.Divide -> match stack with
                        | n::m::xs when m = 0 -> 0 :: xs, cc, dp   //Ignore division by zero
                        | n::m::xs -> let s = m / n
                                      s :: xs, cc, dp 
                        | _ -> failwith("Stack is empty")

    | command.Greater -> match stack with
                         | n::m::xs when m > n -> 1 :: xs, cc, dp 
                         | n::m::xs -> 0 :: xs, cc, dp 
                         | _ -> failwith("Stack is empty")

    | command.Switch ->  match stack with
                         | [] -> failwith("Stack is empty")
                         | x::xs when x % 2 = 0 -> xs, cc, dp 
                         | x::xs -> xs, (toggle cc), dp

    //| command.Pointer ->  match stack with
    //                      | [] -> failwith("Stack is empty")
    //                      | x::xs when x > 0 -> xs, cc, dp 
    //                      | x::xs -> xs, cc, dp

    //| command.Roll -> match stack with
    //                  | n::d::xs -> let xs' = rollStack xs d n
    //                                xs', cc, dp 
    //                  | _ -> failwith("Stack is empty")

    | _ -> failwith(sprintf "Unknown command %A" cmd)

let rec runLoop program width height x y dp cc stack =
    let next = getNextCommand program width height x y dp cc
    match next with
    | None -> 0
    | Some((x',y'), dp', cc') -> 
                                let previousNumber = countPixelsInBlock program x y width height
                                let cmd = decodeCommand program.[x,y] program.[x',y'] 
                                let stack', cc'', dp'' = doCommand previousNumber cmd stack cc' dp'
                                runLoop program width height x' y' dp'' cc'' stack'

[<EntryPoint>]
let main argv = 
    //let image = new Bitmap("C:\Code\Piet.net\Piet_hello2.png")
    //let width, height, program = loadImage image 1//5

    let image = new Bitmap("C:\Code\Piet.net\Piet_hello_Big.png")
    let width, height, program = loadImage image 5

    let a = runLoop program width height 0 0 dp.Right cc.Left []
    let b = System.Console.ReadLine()
    a