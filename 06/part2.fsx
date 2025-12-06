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

let splitByEmptyColumn (input: list<list<char>>) : list<list<list<char>>> =
    let rec collector input currentOutput totalOutput =
        match input with
        | [] -> currentOutput :: totalOutput
        | head :: tail when List.forall (fun x -> x = ' ') head -> collector tail [] (currentOutput :: totalOutput)
        | head :: tail -> collector tail (head :: currentOutput) totalOutput

    collector input [] []

let applyOperation (numbers: list<int64>) (operation: Operation) =
    numbers
    |> List.reduce (fun a b ->
        match operation with
        | Add -> a + b
        | Multiply -> a * b)

let input = File.ReadAllLines "./06/input.txt"

let operations =
    input |> Array.last |> getLineItems |> Array.map parseOperation |> Array.toList

let numbers =
    input
    |> Array.take (Array.length input - 1)
    |> Array.toList
    |> Seq.transpose
    |> Seq.toList
    |> List.map Seq.toList
    |> splitByEmptyColumn
    |> List.map (
        List.map (
            List.fold
                (fun n x ->
                    match x with
                    | ' ' -> n
                    | c -> n * 10L + (string >> int64) c)
                0L
        )
    )
    |> List.rev

let results =
    List.zip numbers operations |> List.map (fun (num, op) -> applyOperation num op)

printfn "%A" (results |> List.sum)
