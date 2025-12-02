open System.IO

let parseRanges (input: string) =
    input.Split ","
    |> Array.map (fun x -> x.Trim())
    |> Array.map (fun x ->
        match x.Split "-" with
        | [| first; last |] -> (int64 first, int64 last)
        | _ -> failwith "Invalid range")
    |> Array.toSeq

let divisors (n: int) =
    [ 2..n ] |> List.where (fun x -> n % x = 0)

let strParts (partLen: int) (str: string) =
    let rec loop (result: list<string>) (rest: string) =
        match rest with
        | str when (String.length str) = 0 -> result
        | str -> loop (result @ [ rest.[.. partLen - 1] ]) rest.[partLen..]

    loop [] str

let divisorsToStrParts (divisors: int list) (str: string) =
    let strLen = String.length str

    divisors
    |> List.map (fun x ->
        let partLen = strLen / x
        strParts partLen str)

let isDouble (double: string list) =
    match double with
    | [] -> false
    | head :: rest -> rest |> List.forall (fun x -> x = head)

let findDoubles ((first, last)) =
    seq { first..last }
    |> Seq.map string
    |> Seq.map (fun x -> x, divisors (String.length x))
    |> Seq.map (fun (x, divisors) -> x, divisorsToStrParts divisors x)
    |> Seq.where (fun (x, doubles) -> List.exists isDouble doubles)
    |> Seq.map (fun (x, _) -> int64 x)

let result =
    File.ReadAllText "./02/input.txt"
    |> parseRanges
    |> Seq.map findDoubles
    |> Seq.map Seq.sum
    |> Seq.sum

printfn "%A" result
