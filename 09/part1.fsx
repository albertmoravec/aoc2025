open System.IO

let coordinates =
    File.ReadAllLines "./09/input.txt"
    |> Array.map (fun x -> x.Split ',')
    |> Array.map (fun x ->
        match x with
        | [| a; b |] -> int64 a, int64 b
        | _ -> failwith "Invalid point")
    |> Array.toList

let squareArea ((ax, ay), (bx, by)) =
    let distX = abs (ax - bx)
    let distY = abs (ay - by)

    (distX + 1L) * (distY + 1L)

let allSquares =
    seq {
        for index, x in Seq.indexed coordinates do
            for y in Seq.skip index coordinates do
                if x <> y then
                    yield x, y
    }

let largestSquare = allSquares |> Seq.maxBy squareArea

printfn "Result: %A" (squareArea largestSquare)
