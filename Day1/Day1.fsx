open System
open System.Linq

// General
let readLines (filePath:string) = System.IO.File.ReadLines(filePath).ToArray();;


let readValues fileName = 
    readLines fileName    
    |> Array.map(fun value -> string value)
    |> Array.map(fun string -> int string)

// Aufgabe 1

let folder ((sum: int, prev: int), next: int) = if prev < next then (sum + 1, next) else (sum, next)

let calculateIncrements (values: int[]) =
    Array.fold(fun (sum, prev) next -> if prev < next then (sum + 1, next) else (sum, next)) (-1, 0) values

Console.WriteLine("Aufgabe 1:");
Console.WriteLine(calculateIncrements (readValues "input.txt"))

// Aufgabe 2
let createWindow (values: int[], index: int): int[] = values.[index .. index + 2]

let sumSlice (slice): int = Array.fold(fun a b -> a + b) 0 slice


let calculateWindowsIncrement (values: int[]) = 
    Array.map(fun index -> createWindow(values, index)) ([0 .. values.Length - 2].ToArray())
    |> Array.map(fun slice -> sumSlice slice)
    |> calculateIncrements

Console.WriteLine("Aufgabe 2:")
Console.WriteLine(calculateWindowsIncrement (readValues "input.txt"))
    