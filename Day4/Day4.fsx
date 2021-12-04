open System
open System.Linq
open FSharp.Core

let readLines (filePath:string) = System.IO.File.ReadLines(filePath).ToArray()

let inputFile = "input.txt"

let drawnNumbers = 
    readLines inputFile
    |> System.Linq.Enumerable.First    
    |> fun firstRow -> firstRow.Split(',')
    |> Array.map (fun text -> int text)

let bingoBoards = 
    readLines inputFile
    |> fun lines -> lines.Skip 1
    |> fun lines -> lines.Chunk 6
    |> fun list -> list.Select(fun lines -> lines[1 .. lines.Length - 1])        
    |> fun list -> list.Select(fun lines -> lines.Select(fun line -> line.Split(' ')))
    |> fun list -> list.Select(fun lines -> lines.Select(fun line -> line.Where(fun value -> value <> "").ToArray()).ToArray())
    |> fun list -> list.ToArray()
    |> Array.map (Array.map (Array.map (fun value -> int value)))
    |> fun array -> array.Select(
        fun innerArray -> Array2D.init innerArray.Length innerArray[0].Length (fun x y -> innerArray[x][y])
    )
    |> fun collection -> collection.ToArray()

let setDrawnNumber board number =
    Array2D.map (fun value -> if value = number then 0 else value) board

let sumArray (array: int[]) =
    Array.sum array    

let emptyBoard = Array2D.init 0 0 (fun x y -> 0)

let dropFirstRow(board: int[,]): int[,] option =
    if Array2D.length1 board > 1 then Some(board.[1 .. Array2D.length1 board,0 .. Array2D.length2 board]) else None

let dropFirstColumn(board: int[,]): int[,] option =
    if Array2D.length2 board > 1 then Some(board.[0 .. Array2D.length1 board, 1 .. Array2D.length2 board]) else None

let rec checkRows (board: int[,] option) = 
    match board with
    | Some board -> if sumArray(board.[0, *]) = 0 then true else checkRows(dropFirstRow board)         
    | None -> false    

let rec checkAllRows (board: int[,] option) =
    match board with
    | Some board -> if sumArray(board.[0, *]) = 0 then checkRows(dropFirstRow board) else false
    | None -> true

let rec checkColoumns (board: int[,] option) =
    match board with
    | Some board -> if sumArray(board.[*, 0]) = 0 then true else checkColoumns(dropFirstColumn board)
    | None -> false    

let rec checkAllColoumns (board: int[,] option) =
    match board with
    | Some board -> if sumArray(board.[*, 0]) = 0 then checkColoumns(dropFirstColumn board) else false
    | None -> true    

let rec checkFullBoard (board: int[,]) =
    checkAllRows (Some board) && checkAllColoumns (Some board)

let checkWinCondition board =
    checkRows (Some board) || checkColoumns (Some board)

let rec firstBingoBoard (boards: int[,][]): int[,] option =
    match checkWinCondition boards[0] with
    | true -> Some(boards[0])
    | false -> if boards.Length <> 1 then firstBingoBoard boards[1 .. boards.Length - 1] else None        



let rec sumBoard (board: int[,] option) =
    match board with
    | Some board -> (sumArray board.[0, *]) + sumBoard (dropFirstRow board) 
    | None -> 0

let rec firstBingo (drawnNumbers: int[]) (boards: int[,][]) =
    Array.map (fun board -> setDrawnNumber board drawnNumbers[0]) boards
    |> fun list -> match firstBingoBoard list with
                    | Some board -> sumBoard (Some board) * drawnNumbers[0]
                    | None -> firstBingo drawnNumbers[1 .. drawnNumbers.Length - 1] list

Console.WriteLine("Aufgabe 1")
Console.WriteLine(firstBingo drawnNumbers bingoBoards)

let removeBingoBoards (boards: int[,][]): int[,][] =
    Array.filter (fun board -> not (checkWinCondition board)) boards

let rec lastBingo (drawnNumbers: int[]) (boards: int[,][]) =
    Array.map (fun board -> setDrawnNumber board drawnNumbers[0]) boards
    |> removeBingoBoards
    |> fun list -> match list.Length = 0 with
                    | true -> sumBoard (Some (setDrawnNumber boards[0] drawnNumbers[0])) * drawnNumbers[0]
                    | false -> lastBingo drawnNumbers[1 .. drawnNumbers.Length - 1] list

Console.WriteLine("Aufgabe 2")
printfn "%A" (lastBingo drawnNumbers bingoBoards)
// Console.WriteLine(lastBingo drawnNumbers bingoBoards)