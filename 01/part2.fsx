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

let performTurn (startPosition: int) (turn: Turn) : int * int =
    let fullRotations = turn.Steps / 100

    let stepsToDo =
        match turn.Direction with
        | Left -> -(turn.Steps % 100)
        | Right -> turn.Steps % 100

    let finishPosition = (startPosition + stepsToDo + 100) % 100

    let passedZero =
        if
            startPosition <> 0 && finishPosition = 0
            || startPosition <> 0 && turn.Direction = Left && finishPosition > startPosition
            || startPosition <> 0 && turn.Direction = Right && finishPosition < startPosition
        then
            1
        else
            0

    finishPosition, fullRotations + passedZero

let position, zeroes =
    File.ReadLines "./01/input.txt"
    |> Seq.map turnParser
    |> Seq.fold
        (fun (startPosition, startZeroes) turn ->
            let finishPosition, zeroesCounted = performTurn startPosition turn
            finishPosition, startZeroes + zeroesCounted)
        (50, 0)

printf "%d %d" position zeroes
