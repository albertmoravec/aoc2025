open System.IO

let inputLines = File.ReadAllLines "./07/input.txt"

let width = inputLines |> Array.head |> String.length
let height = Array.length inputLines

let inputGrid = inputLines |> Array.map Seq.toArray

let updateRowOrIgnore (index: int) (symbol: char) (row: char array) : char array =
    if index < 0 || index > Array.length row - 1 then
        row
    else
        Array.updateAt index symbol row

// Calculates how beam should pass from the previous row into the current one and returns the new resulting row with the beam calculated
let calculateRowBeam (previousRow: char array) (currentRow: char array) : char array =
    currentRow
    |> Array.fold
        (fun (index, currentRow) currentSymbol ->
            match previousRow[index], currentSymbol with
            | '|', '.'
            | 'S', '.' -> index + 1, Array.updateAt index '|' currentRow
            | '|', '^' ->
                index + 1,
                currentRow
                |> updateRowOrIgnore (max 0 (index - 1)) '|'
                |> updateRowOrIgnore (min width (index + 1)) '|'
            | _ -> index + 1, currentRow)
        (0, currentRow)
    |> snd

let countSplittersHit (gridWithBeams: char array array) : int =
    seq {
        for x in 0 .. height - 1 do
            for y in 0 .. width - 1 do
                if x > 0 && gridWithBeams[x][y] = '^' && gridWithBeams[x - 1][y] = '|' then
                    yield 1
    }
    |> Seq.sum

let gridWithBeams =
    inputGrid
    |> Array.fold
        (fun (index, newArray) row ->
            let newRow =
                if index = 0 then
                    row
                else
                    calculateRowBeam (Array.get newArray (index - 1)) row

            index + 1, Array.updateAt index newRow newArray)
        (0, inputGrid)
    |> snd

printfn "%A" (countSplittersHit gridWithBeams)