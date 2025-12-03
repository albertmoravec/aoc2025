open System.IO

let maxJolts (bank: list<int>) : int =
    let remainingBank = bank.[.. List.length bank - 2]
    let pos, firstValue = remainingBank |> List.indexed |> List.maxBy snd
    let secondValue = bank |> List.skip (pos + 1) |> List.max

    firstValue * 10 + secondValue

let banks =
    File.ReadLines "./03/input.txt"
    |> Seq.map (Seq.map (string >> int) >> Seq.toList)

let result = banks |> Seq.map maxJolts |> Seq.sum

printfn "%A" result
