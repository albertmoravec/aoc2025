open System.IO

let input = File.ReadAllText "./05/input.txt"

let freshInput, availableInput =
    match input.Split "\n\n" with
    | [| first; second |] -> first.Trim(), second.Trim()
    | _ -> failwith "Invalid input"

let freshRanges =
    freshInput.Split '\n'
    |> Array.map (fun x -> x.Split "-")
    |> Array.map (Array.map int64)
    |> Array.map (fun x ->
        match x with
        | [| a; b |] when a <= b -> a, b
        | _ -> failwith "Invalid range definition")

let availableSet = availableInput.Split '\n' |> Array.map int64 |> set

let itemInRange x (a, b) = x >= a && x <= b

let availableFresh =
    availableSet |> Seq.where (fun x -> Array.exists (itemInRange x) freshRanges)

printf "%A" (Seq.length availableFresh)
