open System.IO
open System

type Operation =
    | Add
    | Multiply

let parseOperation (opString: string) =
    match opString.Trim() with
    | "+" -> Add
    | "*" -> Multiply
    | _ -> failwith "Invalid operation"

let getLineItems (line: string) =
    line.Split(' ', StringSplitOptions.RemoveEmptyEntries)

let applyOperation (numbers: array<int64>) (operation: Operation) =
    numbers
    |> Array.reduce (fun a b ->
        match operation with
        | Add -> a + b
        | Multiply -> a * b)

let input = File.ReadAllLines "./06/input.txt"

let operations = input |> Array.last |> getLineItems |> Array.map parseOperation

let numbers =
    input
    |> Array.take (Array.length input - 1)
    |> Array.map getLineItems
    |> Array.map (Array.map int64)
    |> Array.transpose

let results =
    Array.zip numbers operations
    |> Array.map (fun (num, op) -> applyOperation num op)

printfn "%A" (results |> Array.sum)
