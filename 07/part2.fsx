open System.IO

let inputLines = File.ReadAllLines "./07/input.txt"
let inputGrid = inputLines |> Array.toList |> List.map Seq.toArray

let rewriteFirstRow (row: char array) : int64 array =
    row
    |> Array.map (fun char ->
        match char with
        | '.' -> 0
        | 'S' -> 1
        | _ -> failwith "Invalid character")

let rewriteRow (previousRow: int64 array) (currentRow: char array) : int64 array =
    currentRow
    |> Array.mapi (fun index char ->
        let isSplitter = char = '^'

        let leftNeighbor = Array.tryItem (index - 1) currentRow
        let rightNeighbor = Array.tryItem (index + 1) currentRow

        let leftIsSplitter = leftNeighbor = Some '^'
        let rightIsSplitter = rightNeighbor = Some '^'

        let valueFromAbove = previousRow[index]
        let valueFromLeft = if leftIsSplitter then previousRow[index - 1] else 0
        let valueFromRight = if rightIsSplitter then previousRow[index + 1] else 0

        if isSplitter then
            0
        else
            valueFromAbove + valueFromLeft + valueFromRight)

let firstRow = rewriteFirstRow (List.head inputGrid)

let result =
    inputGrid
    |> List.tail
    |> List.fold (fun countRow row -> rewriteRow countRow row) firstRow
    |> Array.sum

printfn "%A" result
