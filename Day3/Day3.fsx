open System
open System.Linq
open FSharp.Core

// General
let readLines (filePath:string) = System.IO.File.ReadLines(filePath).ToArray()

let diagnosticReport (lines: string[]): int[][] = 
    Array.map(fun (line: string) ->  line.ToCharArray()) lines
    |> Array.map(Array.map(fun (char: char) -> if char = '0' then 0 else 1))

let reports (inputFile: string) =
    readLines inputFile
    |> diagnosticReport

let convertToArray2D (lines: int[][]) =  Array2D.init lines.Length lines[0].Length (fun x y -> lines[x][y] )

let summedBits report =
    convertToArray2D report
    |> fun array ->  Array.map(fun index -> array[*, index]) ([0 .. (Array2D.length2 array - 1)].ToArray())
    |> Array.map(Array.map(fun x -> if x = 0 then -1 else 1))
    |> Array.map Array.sum    


let mostComonBits (report: int[][]) =
    summedBits report
    |> Array.map (fun x -> if x < 0 then 0 else 1)

let leastComonBits (report: int[][]) = 
    summedBits report
    |> Array.map (fun x -> if x < 0 then 1 else 0)

// Aufgabe 1

let toBinary (bits: int[]) =
    Array.rev bits
    |> Array.mapi (fun value index -> index <<< value)
    |> Array.sum

let gamma =
    readLines "input.txt"
    |> diagnosticReport
    |> mostComonBits
    |> toBinary

let epsilon =
    readLines "input.txt"
    |> diagnosticReport
    |> leastComonBits
    |> toBinary

Console.WriteLine("Aufgabe 1")
Console.WriteLine(gamma * epsilon)

// Aufgabe 2
 
let mostCommonBitByIndex (reports: int[][]) (index: int): int = 
    mostComonBits reports
    |> fun mostComonBits -> mostComonBits[index]

let leastCommonBitByIndex (reports: int[][]) (index: int): int = 
    leastComonBits reports
    |> fun leastCommonBits -> leastCommonBits[index]

let filterByBitByIndex (reports: int[][]) (index: int) (bit) =
    Array.filter (fun (report: int[]) -> report[index] = bit) reports

let folder (reports: int[][]) (index: int): int[][] = 
    mostCommonBitByIndex reports index
    |> filterByBitByIndex reports index

let oxygen =
    reports "input.txt"
    |> fun reports -> List.fold folder reports [0 .. reports[0].Length - 1]
    |> fun report -> report[0]
    |> toBinary

let co2Folder (reports: int[][]) (index: int) =
    leastCommonBitByIndex reports index    
    |> fun bit -> if (filterByBitByIndex reports index bit).Length = 0 then reports else filterByBitByIndex reports index bit

let co2 =
    reports "input.txt"
    |> fun reports -> List.fold co2Folder reports [0 .. reports[0].Length - 1]
    |> fun report -> report[0]
    |> toBinary

Console.WriteLine "Aufgabe 2"
Console.WriteLine(oxygen * co2);