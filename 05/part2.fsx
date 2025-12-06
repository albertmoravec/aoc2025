open System.IO

type Range = (int64 * int64)

let input = File.ReadAllText "./05/input.txt"

let freshInput =
    match input.Split "\n\n" with
    | [| first; _ |] -> first.Trim()
    | _ -> failwith "Invalid input"

let freshRanges: list<Range> =
    freshInput.Split '\n'
    |> Array.toList
    |> List.map (fun x -> x.Split "-")
    |> List.map (Array.map int64)
    |> List.map (fun x ->
        match x with
        | [| a; b |] when a <= b -> a, b
        | _ -> failwith "Invalid range definition")

let rangeLength ((xMin, xMax): Range) = xMax - xMin + 1L

let isInRange ((rangeMin, rangeMax): Range) (x: int64) : bool = x >= rangeMin && x <= rangeMax

let rec intersects (a: Range) (b: Range) : bool =
    let aMin, aMax = a
    let bMin, bMax = b

    isInRange a bMin || isInRange a bMax || isInRange b aMin || isInRange b aMax

// Combines two intersecting ranges into one
let combineRanges (a: Range) (b: Range) : Range =
    let aMin, aMax = a
    let bMin, bMax = b

    if intersects a b then
        min aMin bMin, max aMax bMax
    else
        failwith "Cannot combine non-intersecting ranges"

let sortedRanges = freshRanges |> List.sortBy fst

let countAllRanges (items: list<Range>): int64 =
    let rec counter (count: int64) (item: Range) (rest: list<Range>): int64 = 
        match rest with
        | [] -> count + rangeLength item
        | head::tail ->
            if intersects head item then
                counter count (combineRanges head item) tail
            else
                counter (count + rangeLength item) head tail
    
    match items with
    | [] -> 0
    | head::tail -> counter 0 head tail 


printfn "%A" (countAllRanges sortedRanges)
