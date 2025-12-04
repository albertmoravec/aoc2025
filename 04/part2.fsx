open System.IO

let input =
    File.ReadLines "./04/input.txt"
    |> Seq.map (Seq.map (fun x -> if x = '@' then true else false))
    |> array2D

let width, height = Array2D.length1 input, Array2D.length2 input

// Returns 2D array of accessible papers
let accessiblePapers (input: bool array2d) : bool array2d =
    Array2D.init (Array2D.length1 input) (Array2D.length2 input) (fun x y ->
        let minX = max 0 (x - 1)
        let maxX = min (height - 1) (x + 1)
        let minY = max 0 (y - 1)
        let maxY = min (width - 1) (y + 1)

        let neighbors =
            seq {
                for i in minX..maxX do
                    for j in minY..maxY -> input[i, j]
            }

        let neighborCount = neighbors |> Seq.where id |> Seq.length

        // input[x,y] is included in neighborCount
        if input[x, y] && neighborCount <= 4 then true else false)

let accessibleCount (accessible: bool array2d): int = accessible |> Seq.cast<bool> |> Seq.where id |> Seq.length

// Returns new array with accessible papers removed
let removePapers (input: bool array2d) (accessible: bool array2d) : bool array2d =
    Array2D.init (Array2D.length1 input) (Array2D.length2 input) (fun x y ->
        if input[x, y] && not accessible[x, y] then true else false)

// Removes all accessible papers and returns the number of items removed
let removeAllAccessible (input: bool array2d): int =
    let rec removal (input: bool array2d) (removed: int) =
        let accessibleItems = accessiblePapers input
        let currAccessible = accessibleCount accessibleItems

        if currAccessible = 0 then
            removed
        else
            let newInput = removePapers input accessibleItems
            removal newInput (removed + currAccessible)
    
    removal input 0

let result = removeAllAccessible input
printf "%A" result
