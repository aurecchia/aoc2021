open System.IO

let readLines (path: string) =
    seq {
        use reader = new StreamReader (path)
        while not reader.EndOfStream do
            yield reader.ReadLine ()
    }

let countOfDepthIncreases (depths: int seq) =
    depths
    |> Seq.pairwise
    |> Seq.where (fun (fst, snd) -> snd > fst)
    |> Seq.length

let depths =
    readLines "input"
    |> Seq.map int

depths
|> countOfDepthIncreases
|> printfn "%d"

depths
|> Seq.windowed 3
|> Seq.map Array.sum
|> countOfDepthIncreases
|> printfn "%d"
