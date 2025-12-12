open System.IO

let coordinates =
    File.ReadAllLines "./09/demo.txt"
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

let isInRange (rangeMin, rangeMax) (x: int64) : bool = x >= rangeMin && x <= rangeMax

let isInExclusiveRange (rangeMin, rangeMax) (x: int64) : bool = x > rangeMin && x < rangeMax

let rec intersects a b : bool =
    let aMin, aMax = a
    let bMin, bMax = b

    isInRange a bMin || isInRange a bMax || isInRange b aMin || isInRange b aMax

let rec intersectsExclusive a b : bool =
    let aMin, aMax = a
    let bMin, bMax = b

    isInExclusiveRange a bMin
    || isInExclusiveRange a bMax
    || isInExclusiveRange b aMin
    || isInExclusiveRange b aMax


let allSquares =
    seq {
        for index, x in Seq.indexed coordinates do
            for y in Seq.skip index coordinates do
                if x <> y then
                    yield x, y
    }

let lines1 =
    coordinates
    |> List.chunkBySize 2
    |> List.map (fun x ->
        match x with
        | [ x; y ] -> x, y
        | _ -> failwith "Invalid line")

let lines2 =
    coordinates
    |> List.skip 1
    |> List.take (List.length coordinates - 2)
    |> List.chunkBySize 2
    |> List.map (fun x ->
        match x with
        | [ x; y ] -> x, y
        | _ -> failwith "Invalid line")
    |> List.append [ (List.head coordinates, List.head (List.rev coordinates)) ]

let verticalLines, horizontalLines =
    match List.head lines1 with
    | (ax, ay), (bx, by) when ax = bx -> lines1, lines2
    | (ax, ay), (bx, by) when ay = by -> lines2, lines1
    | _ -> failwithf "Invalid line arrays: %A" (List.head lines1)

let orderRange (a, b) = min a b, max a b

let noLinesWithinY minX minY maxX maxY =
    let xRange = minX, maxX
    let yRange = minY, maxY

    horizontalLines
    |> List.where (fun ((ax, ay), (bx, by)) ->
        let lineXRange = orderRange (ax, bx)

        intersectsExclusive xRange lineXRange && isInExclusiveRange yRange ay)
    |> List.isEmpty

let noLinesWithinX minX minY maxX maxY =
    let xRange = minX, maxX
    let yRange = minY, maxY

    verticalLines
    |> List.where (fun ((ax, ay), (bx, by)) ->
        let lineYRange = orderRange (ay, by)

        intersectsExclusive yRange lineYRange && isInExclusiveRange xRange ax)
    |> List.isEmpty

let isWithinY minX minY maxX maxY =
    let xRange = minX, maxX
    let yRange = minY, maxY

    

let validSquares =
    allSquares
    |> Seq.where (fun ((ax, ay), (bx, by)) ->
        let minX = min ax bx
        let maxX = max ax bx

        let minY = min ay by
        let maxY = max ay by

        noLinesWithinY minX minY maxX maxY && noLinesWithinX minX minY maxX maxY)

let largestSquare = validSquares |> Seq.maxBy squareArea

printfn "All squares: %A" (Seq.length allSquares)
printfn "Valid squares: %A" (Seq.length validSquares)
printfn "Largest square: %A" largestSquare
printfn "Largest square area: %A" (squareArea largestSquare)
