open System
open System.Linq
open FSharp.Core

let readLines (filePath:string) = System.IO.File.ReadLines(filePath)

let intialFishs = 
    Seq.cast<string> (readLines "input.txt")
    |>  Seq.head
    |> fun (string: string) -> string.Split(',').ToList()
    |> Seq.map (fun value -> int value)

let evolution (currentFishs: seq<int>) =
    currentFishs
    |> Seq.map (fun age -> 
        match age with
        | 0 -> [6; 8]
        | x -> [x - 1]
    )
    |> Seq.collect (fun list -> list)

// Aufgabe 1

let createFishs (initialFishs: seq<int>) (maxCycle: int) =
    Seq.unfold (fun (currentFishs: seq<int>) -> Some(evolution currentFishs, evolution currentFishs)) initialFishs
    |> Seq.take maxCycle
    |> Seq.last
    |> Seq.length

Console.WriteLine("Aufgabe 1")
Console.WriteLine(createFishs intialFishs 80)

// Aufgabe 2
        
let groupFishs (fishs: seq<int>) =
    let mutableArray = Array.zeroCreate 9

    for age in fishs do
        mutableArray[age] <- mutableArray[age] + 1

    Array.map (fun amount -> int64 amount) mutableArray

let performEvoultionOnGroup (fishs: int64[]) =
    let mutableArray = Array.zeroCreate 9

    for age in 0 .. fishs.Length - 1 do
        if age = 0 then
            mutableArray[6] <- fishs[0]; mutableArray[8] <- fishs[0]
        else
            mutableArray[age - 1] <- mutableArray[age - 1] + fishs[age]    
    mutableArray        

let evoluteForDays (fishs: seq<int>) (days: int) =
    groupFishs fishs
    |> fun grouped -> Seq.fold (fun fishs _ -> performEvoultionOnGroup fishs) grouped [1 .. days]
    |> Array.sum

Console.WriteLine("Aufgabe 2")
Console.WriteLine(evoluteForDays intialFishs 265)