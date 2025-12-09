open System.IO

type Junction = int64 * int64 * int64

let parseJunction (line: string) : Junction =
    let coords = line.Split ','

    match coords with
    | [| x; y; z |] -> int64 x, int64 y, int64 z
    | _ -> failwith "Invalid junction box definition"

let junctionDistance (a: Junction) (b: Junction) : float =
    let ax, ay, az = a
    let bx, by, bz = b

    sqrt (float (pown (bx - ax) 2 + pown (by - ay) 2 + pown (bz - az) 2))

let junctions =
    File.ReadAllLines "./08/input.txt" |> Array.toList |> List.map parseJunction

let allPairs =
    set (
        seq {
            for index, x in Seq.indexed junctions do
                for y in Seq.skip index junctions do
                    if x <> y then
                        yield x, y
        }
    )

let closestPairs = allPairs |> Seq.sortBy (fun (a, b) -> junctionDistance a b)
let unconnectedCircuits = List.map (fun x -> set [ x ]) junctions

let connectedCircuits, lastPair =
    closestPairs
    |> Seq.fold
        (fun (circuits: Set<Junction> list, lastPair: (Junction * Junction) option) (a, b) ->
            let indexA = List.findIndex (Set.contains a) circuits
            let indexB = List.findIndex (Set.contains b) circuits

            if List.length circuits = 1 then
                circuits, lastPair
            else if indexA = indexB then
                circuits, lastPair
            else
                let indexA = List.findIndex (Set.contains a) circuits
                let setA = List.item indexA circuits

                let circuitsWithoutA = List.removeAt indexA circuits

                let indexB = List.findIndex (Set.contains b) circuitsWithoutA
                let setB = List.item indexB circuitsWithoutA

                let newSet = Set.union setA setB
                let updatedCircuits = List.updateAt indexB newSet circuitsWithoutA

                updatedCircuits, Some(a, b))
        (unconnectedCircuits, None)

let (ax, ay, az), (bx, by, bz) = Option.get lastPair

printfn "Result: %A" (ax * bx)