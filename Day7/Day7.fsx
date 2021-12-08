open System

let readLines (filePath:string) = System.IO.File.ReadLines(filePath)

let depths = 
    readLines "input.txt"
    |> Seq.head
    |> fun string -> string.Split(',')
    |> Seq.map (fun value -> int value)

let averageDepth (depths: seq<int>) =    
    Seq.sort depths
    |> Seq.sum
    |> fun sum -> int (Math.Round(float sum / float (Seq.length depths)))

let depthDifference (depth1: int) (depth2: int) =
    System.Math.Abs (depth1 - depth2)

let shortestDepth (depths: seq<int>) =
    Seq.map (fun depth -> Seq.map (depthDifference depth) depths) depths
    |> Seq.map Seq.sum
    |> Seq.mapi (fun index value -> (index, value))
    |> Seq.minBy (fun (index, value) -> value)
    |> fst
    |> fun index -> (Seq.toArray depths)[index]

// Aufgabe 1

let fuelConsumptionChallenge1 (depths: seq<int>) (alignmentDepth: int) =
    Seq.map (depthDifference alignmentDepth) depths
    |> Seq.sum

Console.WriteLine("Aufgabe 1")
Console.WriteLine(fuelConsumptionChallenge1 depths (shortestDepth depths))

// Aufgabe 2

let gaussSum (n: int) = int (((float n ** 2) + float n)/ float 2)

let fuelConsumptionChallenge2 (depths: seq<int>) =     
    Seq.map (fun depth -> Seq.map (depthDifference depth) depths) [0 .. Seq.max depths]
    |> Seq.map (Seq.map gaussSum)
    |> Seq.map Seq.sum
    |> Seq.mapi (fun index value -> (index, value))
    |> Seq.minBy (fun (index, value) -> value)
    |> fst
    |> fun alignmentDepth -> Seq.map (depthDifference alignmentDepth) depths
    |> Seq.map gaussSum
    |> Seq.sum    

    
    
printfn "%A" (fuelConsumptionChallenge2 depths)
// printfn "%A" (Seq.toArray (fuelConsumptionChallenge2 depths))