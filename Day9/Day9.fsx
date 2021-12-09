open System
open System.Collections.Generic

let readLines (filePath:string) = System.IO.File.ReadLines(filePath)

let matrix = 
    readLines "input.txt"
    |> Seq.map (fun (string: string) -> Seq.toArray string)
    |> Seq.map (Seq.map (fun (char: char) -> int char - int '0'))
    |> Seq.map (Seq.toArray)
    |> Seq.toArray
    |> fun seq -> Array2D.init (Seq.length seq) (seq[0].Length) (fun x y -> seq[x][y])


let combinations (xValues: seq<int>) (yValues: seq<int>) =
    Seq.map (fun x -> Seq.map(fun y -> (x, y)) yValues) xValues

let foldArray2D (array: int[,]) =
    Seq.map (fun row -> array.[*, row]) [0 .. Array2D.length1 array - 1]
    |> Seq.concat    
    |> Seq.fold (fun sum next -> sum + next) 0

let array2DtoSequence<'T> (array: 'T[,]) = 
    Seq.map (fun row -> array.[*, row]) [0 .. Array2D.length1 array - 1]
    |> Seq.concat    

let filteriArray2D (condition: int * int * int -> bool) (array: int[,]) =
    let result = List<int>();
    for x in [0 .. (Array2D.length1 array) - 1] do
        for y in [0 .. (Array2D.length2 array) - 1] do
            let  element = Array2D.get array x y            
            if (condition (x, y, element)) then
                result.Add(element)

    Seq.cast<int> result

let filterMapArray2D(mapper: int * int * int -> (int * int * int) option) (array: int[,]) =
    let result = List<int * int * int>();
    for x in [0 .. (Array2D.length1 array) - 1] do
        for y in [0 .. (Array2D.length2 array) - 1] do
            let  element = Array2D.get array x y
            match mapper (x, y, element) with
            | Some mapped -> result.Add(mapped)
            | None -> ()            
    Seq.cast<int * int * int> result

    
let compactMapArray2D<'Value, 'Result> (mapper: int * int * 'Value -> 'Result option) (array: 'Value[,]) =
    Array2D.mapi (fun x y value -> mapper(x, y, value)) array
    |> array2DtoSequence
    |> Seq.filter (fun value -> 
        match value with
        | Some _ -> true
        | None -> false
    )
    |> Seq.map Option.get


// Aufgabe 1
let adjacentFields (x: int) (y: int) (matrix: int[,]) =
    ((Seq.filter (fun x -> x >= 0 && x <= (Array2D.length1 matrix) - 1) [x - 1; x + 1]), (Seq.filter (fun y -> y >= 0 && y <= (Array2D.length2 matrix) - 1) [y - 1; y + 1]))
    |> fun (xValues, yValues) -> ((Seq.map (fun x -> (x, y)) xValues), (Seq.map (fun y -> (x, y)) yValues))
    |> fun (lhs, rhs) -> Seq.append lhs rhs    

let adjacentValues (x: int) (y: int) (matrix: int[,])  =    
    adjacentFields x y matrix
    |> Seq.map (fun (x, y) -> Array2D.get matrix x y)

let lowPoints (matrix: int[,]) =
    filterMapArray2D (fun (x, y, value) ->         
        match Seq.forall (fun adjacentValue -> value < adjacentValue) (adjacentValues x y matrix) with
        | true -> Some (x, y, value)
        | false -> None
    ) matrix    

let riskLevels (lowPoints: seq<int>) =
    Seq.map (fun lowPoint -> lowPoint + 1) lowPoints

let challenge1 =
    matrix
    |> lowPoints
    |> Seq.map (fun (_, _, value) -> value)
    |> riskLevels
    |> Seq.sum

Console.WriteLine("Aufgabe 1")
Console.WriteLine(challenge1)

let countMarked (matrix: int[,]) = 
    filteriArray2D (fun (_, _, value) -> value = -1) matrix
    |> Seq.length

let adjacentFieldsToMark (x: int) (y: int) (matrix: int[,]) =
    adjacentFields x y matrix
    |> Seq.filter (fun (x, y) -> Array2D.get matrix x y <> -1 && Array2D.get matrix x y <> 9)

let markBasin (lowPoint: (int * int)) (matrix: int[,]) =
    let rec spreadBasin (matrix: int[,]) =
        let mutableMatrix = Array2D.copy matrix

        for x in [0 .. Array2D.length1 matrix - 1] do
            for y in [0 .. Array2D.length2 matrix - 1] do 
                if Array2D.get matrix x y = -1 then
                    for (x, y) in adjacentFieldsToMark x y matrix do
                        Array2D.set mutableMatrix x y -1

        if countMarked mutableMatrix = countMarked matrix then            
            mutableMatrix
        else
            spreadBasin mutableMatrix
    
    let mutableMatrix = Array2D.copy matrix
    Array2D.set mutableMatrix (fst lowPoint) (snd lowPoint) -1
    spreadBasin mutableMatrix
    
let caluclateBasinSize (lowPoint: (int * int)) (matrix: int[,]) =
    markBasin lowPoint matrix
    |> countMarked

let challenge2 =
    matrix
    |> lowPoints
    |> Seq.map (fun (x, y, _) -> caluclateBasinSize (x, y) matrix)    
    |> Seq.sort
    |> Seq.rev
    |> Seq.take 3
    |> Seq.fold (fun acc value -> acc * value) 1

Console.WriteLine("Aufgabe 2")
Console.WriteLine(challenge2)