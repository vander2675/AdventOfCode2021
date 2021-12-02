open System
open System.Linq

// General
let readLines (filePath:string) = System.IO.File.ReadLines(filePath).ToArray();;

let parseCommands (values: string[]) =    
    Array.map (fun (commandString: string) -> commandString.Split(' ') )  values
    |> Array.map (fun (splittedCommandString: string[]) -> (splittedCommandString[0], int splittedCommandString[1]))

// Aufgabe 1

let folder (position: int, depth: int) (command: string, value: int) = 
    match command with
    | "forward" -> (position + value, depth)
    | "down" -> (position, depth + value)
    | "up" -> (position, depth - value)
    | _ -> (position, depth)


let calculatePosition (commandStrings: string[]) = 
    parseCommands commandStrings
    |> Array.fold folder (0, 0)
    |> Math.BigMul

Console.WriteLine("Aufgabe 1")
Console.WriteLine(calculatePosition (readLines "input.txt"))

// Aufgabe 2

let folder2 (position: int, depth: int, aim: int) (command: string, value: int) =
    match command with
    | "forward" -> (position + value, depth + aim * value, aim)
    | "down" -> (position, depth, aim + value)
    | "up" -> (position, depth, aim - value)
    | _ -> (position, depth, aim)

let calculateNewPosition (commandStrings: string[]) =
    parseCommands commandStrings
    |> Array.fold folder2 (0, 0, 0)
    |> (fun (x: int, y:int, z: int)  -> (x, y))
    |> Math.BigMul

Console.WriteLine("Aufgabe 2")
Console.WriteLine(calculateNewPosition (readLines "input.txt"))