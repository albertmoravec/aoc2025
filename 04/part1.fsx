open System.IO

let input =
    File.ReadLines "./04/input.txt"
    |> Seq.map (Seq.map (fun x -> if x = '@' then true else false))
    |> array2D

let width, height = Array2D.length1 input, Array2D.length2 input

let accessible =
    [ for x in 0 .. (width - 1) do
          for y in 0 .. (height - 1) do
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
              if input[x, y] && neighborCount <= 4 then true else false ]
    |> Seq.where id

let result = Seq.length accessible
printf "%A" result
