open System.IO

let parseRanges (input: string) =
    input.Split ","
    |> Array.map (fun x -> x.Trim())
    |> Array.map (fun x ->
        match x.Split "-" with
        | [| first; last |] -> (int64 first, int64 last)
        | _ -> failwith "Invalid range")
    |> Array.toSeq

let isEvenLength (str: string) = String.length str % 2 = 0

let findDoubles ((first, last)) =
    seq { first..last }
    |> Seq.map string
    |> Seq.where isEvenLength
    |> Seq.map (fun x ->
        let halfLen = ((String.length x) / 2)

        let firstHalf = x.[.. halfLen - 1]
        let secondHalf = x.[halfLen..]

        x, firstHalf, secondHalf)
    |> Seq.where (function
        | _, x, y when x = y -> true
        | _ -> false)
    |> Seq.map (fun (x, _, _) -> int64 x)

let input = File.ReadAllText "./02/demo.txt" |> parseRanges

printfn "%A" input

let result = input |> Seq.map findDoubles |> Seq.map Seq.sum |> Seq.sum

printfn "%A" result
