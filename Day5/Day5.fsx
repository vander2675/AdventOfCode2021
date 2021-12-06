open System
open System.Linq
open FSharp.Core

let readLines (filePath:string) = System.IO.File.ReadLines(filePath).ToArray()

[<StructuredFormatDisplay("({x}, {y})")>]
type Point(x: int, y: int) =
    member this.x = x
    member this.y = y
[<StructuredFormatDisplay("{start} -> {stop}")>]
type Line(start: Point, stop: Point) =
    member this.start = start
    member this.stop = stop

let inputFile = readLines "input.txt"

let points = 
    inputFile
    |> Array.map (fun string -> string.Split(" -> "))
    |> Array.map (Array.map (fun string -> string.Split(",")))
    |> Array.map (Array.map (Array.map (fun value -> int value)))
    |> Array.map (Array.map (fun pair -> Point(pair[0], pair[1]))) 

let lines = 
    points
    |> Array.map (fun pair -> Line(pair[0], pair[1]))

let maxX =
    points
    |> Array.map (Array.map (fun point -> point.x))
    |> Array.map Array.max
    |> Array.max
    |> fun max -> max + 1

let maxY =
    points
    |> Array.map (Array.map (fun point -> point.y))
    |> Array.map Array.max
    |> Array.max
    |> fun max -> max + 1

let field () =
    Array2D.init maxX maxY (fun _ _ -> 0)
    |> Array2D.copy

let spread (a: int) (b: int) =
    match a <= b with
        | true -> [a .. b].ToArray()
        | false -> (List.rev [b .. a]).ToArray()

let normalizeArrays (v1: int[]) (v2: int[]) =
    match (v1.Length, v2.Length) with    
    | (x, y) when x < y -> (Array.init y (fun _ -> v1[0]) , v2)
    | (x, y) when x > y -> (v1, Array.init x (fun _ -> v2[0])) 
    | _ -> (v1, v2)

let normalizeLine (line: Line) = normalizeArrays (spread line.start.x line.stop.x) (spread line.start.y line.stop.y)

let applyPointOnField (field: int[,]) (x: int, y: int): int[,] =   
    let mutableField = field
    (Array2D.set mutableField x y ((Array2D.get field x y) + 1))
    mutableField
    // Array2D.init (Array2D.length1 field) (Array2D.length2 field) 
    //     (fun x1 y1 -> if x = x1 && y = y1 then (Array2D.get field x1 y1) + 1 else Array2D.get field x1 y1)

let applyLineOnField (field: int[,]) (line: Line) =
    normalizeLine line
    |> fun tuple -> Array.zip (fst tuple) (snd tuple)
    |> Array.fold (fun (field: int[,]) (point) -> applyPointOnField field point) field

let array2dFold folder (state:'State) (source:'T[,]) =
      source
      |>   ( Seq.cast<'T> >> Seq.fold folder state )

let caluclateCrossovers (field: int[,]) (lines: Line[]) =    
    Array.fold (fun (field: int[,]) (line: Line) -> applyLineOnField field line) field lines
    |> array2dFold (fun (state: int) (value: int) -> if value > 1 then state + 1 else state) 0


// Aufgabe 1

let crossovers1 =
    Array.filter (fun (line: Line) -> (line.start.x = line.stop.x) || (line.start.y = line.stop.y)) lines
    |> caluclateCrossovers (field ())

Console.WriteLine("Aufgabe 1")
Console.WriteLine(crossovers1)

// Aufgabe 2

let crossovers2 = 
    caluclateCrossovers (field ()) lines

Console.WriteLine("Aufgabe 2")
Console.WriteLine(crossovers2)