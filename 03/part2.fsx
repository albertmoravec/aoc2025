open System.IO

let maxJolts (batteryCount: int) (bank: list<int64>) : seq<int64> =
    let bankSize = List.length bank

    let rec maxVoltage (skip: int) (count: int) (result: list<int64>) =
        if count = 0 then
            result
        else
            let remainingBank = bank.[skip .. (bankSize - count)]
            let pos, max = remainingBank |> List.indexed |> List.maxBy snd

            maxVoltage (skip + pos + 1) (count - 1) (result @ [ max ])

    maxVoltage 0 batteryCount []

let bankJoltage (bankJolts: seq<int64>) =
    bankJolts |> Seq.reduce (fun n x -> n * 10L + x)

let banks =
    File.ReadLines "./03/input.txt"
    |> Seq.map (Seq.map (string >> int64) >> Seq.toList)

let result = banks |> Seq.map (maxJolts 12) |> Seq.map bankJoltage |> Seq.sum

printfn "%A" result
