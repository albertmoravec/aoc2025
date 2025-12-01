open System.IO

type Direction =
    | Left
    | Right

type Turn = { Direction: Direction; Steps: int }

let turnParser (instruction: string) : Turn =
    match Seq.toList instruction with
    | 'L' :: steps ->
        { Direction = Left
          Steps = int (steps |> Array.ofList |> System.String.Concat) }
    | 'R' :: steps ->
        { Direction = Right
          Steps = int (steps |> Array.ofList |> System.String.Concat) }
    | _ -> failwith "Invalid turn command"

let position, zeroes =
    File.ReadLines "./01/input.txt"
    |> Seq.map turnParser
    |> Seq.fold
        (fun (position, zeroes) command ->
            match command.Direction with
            | Left -> position - command.Steps, if (position - command.Steps) % 100 = 0 then zeroes + 1 else zeroes
            | Right -> position + command.Steps, if (position + command.Steps) % 100 = 0 then zeroes + 1 else zeroes)
        (50, 0)

printf "%d %d" position zeroes
