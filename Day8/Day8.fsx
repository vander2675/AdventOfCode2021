open System
open System.Collections.Generic

let readLines (filePath:string) = System.IO.File.ReadLines(filePath)

let splittedInputOutput = 
    readLines "input.txt"
    |> Seq.map (fun entry -> entry.Split('|'))
    |> Seq.map (fun array -> (array[0], array[1]))
    |> Seq.map (fun (lhs, rhs) -> (lhs.Trim(), rhs.Trim()))

let outputs = 
    splittedInputOutput
    |> Seq.map snd
    |> Seq.map (fun entry -> entry.Split(' '))

let toCharArrays (value: string) =
    value.Split(' ')
    |> Seq.map (fun (string: string) -> string.ToCharArray())

let inputOutputLines =
    splittedInputOutput    
    |> Seq.map (fun (left, right) -> (toCharArrays left, toCharArrays right))
    
// Aufgabe 1

let countedUniqueNumbers = 
    outputs
    |> Seq.map (Seq.filter (fun entry -> 
        match entry.Length with
        | 2 | 3 | 4  | 7 -> true
        | _ -> false      
    ))
    |> Seq.map Seq.length
    |> Seq.sum

Console.WriteLine("Aufgabe 1")
Console.WriteLine(countedUniqueNumbers)

// Aufgabe 2

let get3 (fiveCharacters: seq<char[]>) =    

    Seq.mapi (fun (index: int) (array: char[]) -> Seq.map (fun (innerSet: Set<char>) -> (index, Set.intersect (Set.ofArray array) innerSet)) (Seq.map Set.ofArray fiveCharacters)) fiveCharacters
    |> Seq.filter (Seq.forall (fun (_, chars) -> (Seq.length chars) = 4 || (Seq.length chars) = 5))
    |> Seq.head
    |> Seq.head
    |> fst
    |> Array.get (Seq.toArray fiveCharacters)

let get9 (sixCharacters: seq<char[]>) (three: char[]) =
    Seq.mapi (fun (index: int) (array: char[]) -> (index, Set.difference (Set.ofArray array) (Set.ofArray three)))  sixCharacters
    |> Seq.filter (fun (index, chars) -> (Seq.length chars) = 1)
    |> Seq.head
    |> fst
    |> Array.get (Seq.toArray sixCharacters)    
    
let get6And0 (sixCharacters: seq<char[]>) (one: char[]) =
    Seq.mapi (fun (index: int) (array: char[]) -> (index, Set.difference (Set.ofArray array) (Set.ofArray one)))  sixCharacters
    |> Seq.sortBy (fun (_, chars) -> Seq.length chars)
    |> Seq.map (fun (index, chars) -> 
        if chars.Count = 4 then
            ((Seq.toArray sixCharacters)[index], 0)
        else
            ((Seq.toArray sixCharacters)[index], 6)
    )
    |> Seq.sortBy (fun (_, value) -> value)
    |> Seq.map (fun (chars, _) -> chars)

let get2and5 (fiveCharacters: seq<char[]>) (four: char[]) =
    Seq.mapi (fun (index: int) (array: char[]) -> (index, Set.difference (Set.ofArray array) (Set.ofArray four)))  fiveCharacters
    |> Seq.sortBy (fun (_, chars) -> Seq.length chars)
    |> Seq.map (fun (index, chars) -> 
        if chars.Count = 2 then
            ((Seq.toArray fiveCharacters)[index], 5)
        else
            ((Seq.toArray fiveCharacters)[index], 2)            
    )
    |> Seq.sortBy (fun (_, value) -> value)
    |> Seq.map (fun (chars, _) -> chars)

let filterByLength (length: int) (codes: seq<char[]>) = Seq.filter (fun (code: char[]) -> Seq.length code = length) codes
let filterByCharArray (charArray: char[]) (codes: seq<char[]>) = Seq.filter (fun code -> code <> charArray) codes

let charArrayToString (array: char[]) = Seq.fold (fun state next -> state + next.ToString()) "" array

let sumCharArray (array: char[]) = 
    Seq.map (fun value -> 
        match value with
        | 'a' -> 1
        | 'b' -> 10
        | 'c' -> 100
        | 'd' -> 1000
        | 'e' -> 10000
        | 'f' -> 100000
        | 'g' -> 1000000
        | _ -> 0
    ) array
    |> Seq.sum

let getNumbers (codes: seq<char[]>) =
    let one = Seq.head (filterByLength 2 codes)
    let four = Seq.head (filterByLength 4 codes)
    let seven = Seq.head (filterByLength 3 codes)
    let eight = Seq.head (filterByLength 7 codes)
    let three = get3 (filterByLength 5 codes)
    let nine = get9 (filterByLength 6 codes) (three)
    let sixAndZero = Seq.toArray( get6And0 (filterByCharArray (nine) (filterByLength 6 codes)) (one))
    let twoAndFive = Seq.toArray( get2and5 (filterByCharArray (three) (filterByLength 5 codes)) (four))

    let dict = Dictionary<int, int>()
    dict.Add(sumCharArray(sixAndZero[0]), 0)
    dict.Add(sumCharArray(one), 1)
    dict.Add(sumCharArray(twoAndFive[0]), 2)
    dict.Add(sumCharArray(three), 3)
    dict.Add(sumCharArray(four), 4)
    dict.Add(sumCharArray(twoAndFive[1]), 5)
    dict.Add(sumCharArray(sixAndZero[1]), 6)
    dict.Add(sumCharArray(seven), 7)
    dict.Add(sumCharArray(eight), 8)
    dict.Add(sumCharArray(nine), 9)

    dict

let calcResult (codes: seq<char[]>) (values: seq<char[]>) = 
    getNumbers codes
    |> fun (decoded: Dictionary<int, int>) -> Seq.map (fun (value: char[]) -> decoded[sumCharArray value]) values
    |> Seq.rev
    |> Seq.mapi (fun (index) (value) -> value * int (float 10 ** index))
    |> Seq.sum

let calcResults (lines: seq<seq<char[]> * seq<char[]>>) =
    Seq.map (fun (lhs, rhs) ->  calcResult lhs rhs) lines
    |> Seq.sum

Console.WriteLine("Aufgabe 2")
Console.WriteLine(calcResults inputOutputLines)
