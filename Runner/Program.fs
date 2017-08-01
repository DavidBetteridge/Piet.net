open Types
open Parser

let rotateStack (stack:List<int>) (depth:int) =
    let first = stack.Head
    let popped = stack.GetSlice (Some(1), Some(depth - 1))
    let rest = stack.GetSlice (Some(depth), None)
    let withTop = first :: rest
    withTop|> List.append popped 

let reverseRotateStack (stack:List<int>) (depth:int) =
    let popped = stack.GetSlice (Some(0), Some(depth - 2))
    let first = stack.Item (depth - 1)
    let rest = stack.GetSlice (Some(depth), None)
    let withTop = first :: popped
    rest |> List.append withTop

let rec rollStack (stack:List<int>) (depth:int) (timesToRoll:int) =
    match depth < 0, timesToRoll with
    | true, _ -> stack   //A negative depth is an error and the command is ignored.
    | _, 0 -> stack
    | _, _ when timesToRoll > 0 -> rollStack (rotateStack stack depth) depth (timesToRoll - 1)
    | _, _ -> rollStack (reverseRotateStack stack depth) depth (timesToRoll + 1)  //A negative number of rolls rolls in the opposite direction

let rec rotateDP (dp:dp) times = 
    if times = 0 then 
        dp
    else if times > 0 then
        rotateDP (moveDPClockwise dp) (times - 1)
    else
        rotateDP (moveDPAntiClockwise dp) (times + 1)


// Pushes the value of the colour block just exited on to the stack. Note that values of colour blocks are not automatically pushed on to the stack - this push operation must be explicitly carried out.
let push previousNumber stack cc dp = 
    previousNumber :: stack, cc, dp    

// Pops the top value off the stack and discards it.
let pop stack cc dp = 
    match stack with
    | [] -> failwith("Stack is empty")
    | x::xs -> xs, cc, dp 

//Pops the top two values off the stack, adds them, and pushes the result back on the stack
let add stack cc dp = 
    match stack with
    | n::m::xs -> let s = n + m
                  s :: xs, cc, dp 
    | _ -> failwith("Stack is empty")

//Pops the top two values off the stack, calculates the second top value minus the top value, and pushes the result back on the stack.
let subtract stack cc dp = 
    match stack with
    | n::m::xs -> let s = m - n
                  s :: xs, cc, dp 
    | _ -> failwith("Stack is empty")

// Pops the top two values off the stack, multiplies them, and pushes the result back on the stack.
let multiply stack cc dp = 
    match stack with
    | n::m::xs -> let s = m * n
                  s :: xs, cc, dp 
    | _ -> failwith("Stack is empty")
    
let doCommand previousNumber cmd stack (cc:cc) (dp:dp) = 
    match cmd with
    | command.NOP -> stack, cc, dp
    | command.Push -> push previousNumber stack cc dp
    | command.Pop -> pop stack cc dp
    | command.Add -> add stack cc dp
    | command.Subtract -> subtract stack cc dp
    | command.Multiply -> multiply stack cc dp

    | command.Not -> match stack with
                     | [] -> failwith("Stack is empty")
                     | x::xs when x = 0 -> 1::xs, cc, dp 
                     | x::xs -> 0::xs, cc, dp


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
                     | _ -> stack, cc, dp //failwith("Stack is empty")
                     




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
                         | [] ->stack, cc, dp// failwith("Stack is empty")
                         | x::xs when x % 2 = 0 -> xs, cc, dp 
                         | x::xs -> xs, (toggle cc), dp

    | command.Pointer ->  match stack with
                          | [] -> failwith("Stack is empty")
                          | x::xs -> xs, cc, (rotateDP dp x)

    | command.Roll -> match stack with
                      | n::d::xs -> let xs' = rollStack xs d n
                                    xs', cc, dp 
                      | _ -> failwith("Stack is empty")

    | command.InNumber -> let value = System.Console.ReadKey().KeyChar
                          let number = (value |> int) - ('0' |> int)
                          (number :: stack), cc, dp

    | command.InChar -> let value = System.Console.ReadKey().KeyChar
                        let number = (value |> int)
                        (number :: stack), cc, dp

    | command.OutNumber -> //Pop the value from the top of the stack and print it.
                           match stack with
                           | [] -> failwith("Stack is empty")
                           | x::xs -> printf "%A" ((x  + ('0' |> int)) |> char)
                                      xs, cc, dp
                                      
let rec runLoop program width height x y dp cc stack =
    let next = getNextCommand program width height x y dp cc
    match next with
    | None -> 0  //end of application
    | Some((x',y'), dp', cc', true) -> 
                                        //Moved from colour block to colour block
                                        let previousNumber = countPixelsInBlock program x y width height
                                        let cmd = decodeCommand program.[x,y] program.[x',y'] 
                                        let stack', cc'', dp'' = doCommand previousNumber cmd stack cc' dp'
                                        runLoop program width height x' y' dp'' cc'' stack'

    | Some((x',y'), dp', cc', false) -> //Moved from colour block to colour block via a white block
                                        runLoop program width height x' y' dp' cc' stack


[<EntryPoint>]
let main argv = 
    //let image = new Bitmap("C:\Code\Piet.net\Piet_hello2.png")
    //let image = new Bitmap("C:\Code\Piet.net\piet_pi.png")
    //let width, height, program = loadImage image 1
    //
    //let image = new Bitmap("C:\Code\Piet.net\Piet_hello_Big.png")
    let width, height, program = loadImage "C:\Code\Piet.net\Piet_hello_Big.png" 5

    let a = runLoop program width height 0 0 dp.Right cc.Left []
    let b = System.Console.ReadLine()
    a