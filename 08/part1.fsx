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

let connectedCircuits =
    closestPairs
    |> Seq.take 1000
    |> Seq.fold
        (fun (circuits: Set<Junction> list) (a, b) ->
            let indexA = List.findIndex (Set.contains a) circuits
            let indexB = List.findIndex (Set.contains b) circuits

            if indexA = indexB then
                // Pair already in the same circuit, do nothing and continue
                circuits
            else
                // Find set where junction A is and remove it from list of circuits
                // Then find set where junction B is and perform union with A set
                // Then update original B set with the new AB union

                let indexA = List.findIndex (Set.contains a) circuits
                let setA = List.item indexA circuits

                let circuitsWithoutA = List.removeAt indexA circuits

                let indexB = List.findIndex (Set.contains b) circuitsWithoutA
                let setB = List.item indexB circuitsWithoutA

                let newSet = Set.union setA setB
                let updatedCircuits = List.updateAt indexB newSet circuitsWithoutA

                updatedCircuits)
        unconnectedCircuits

let circuitsBySize = connectedCircuits |> List.sortByDescending Set.count

printfn "Result: %A" (circuitsBySize |> List.take 3 |> List.map Set.count |> List.reduce (*))
