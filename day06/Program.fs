open System
open System.Collections.Generic
open System.IO

let readLines (path: string) =
    seq {
        use reader = new StreamReader(path)
        while not reader.EndOfStream do
            yield reader.ReadLine()
    }

let numberOfDescendantsAfterNthGeneration =
    let memo = Dictionary<int * int, int64>()
    let rec descendants (generation: int) (fish: int) =
        if generation < 1 then
            1L
        else
            let key = (fish, generation)

            match memo.TryGetValue(key) with
            | true, number -> number
            | _ ->
                let res =
                    if fish = 0 then
                        descendants (generation - 1) 6 + descendants (generation - 1) 8
                    else
                        descendants (generation - 1) (fish - 1)
                memo.Add(key, res)
                res

    descendants
    (*
      descendants(0, 4) =
    = descendants(6, 3) + descendants(8, 3) =
    = descendants(5, 2) + descendants(7, 2) =
    = descendants(4, 1) + descendants(6, 1) =
    = 1                 + 1                 =

    0: 0        6       8
    1: 6 8      5       7
    2: 5 7      4       6
    3: 4 7      3       5
    4: 3 5      2       4
    *)


let run (days: int) (initialFishes: int array) : int64 =
    initialFishes
    |> Seq.map (numberOfDescendantsAfterNthGeneration days)
    |> Seq.map int64
    |> Seq.sum

let parseInput (input: string) : int array =
    input.Split(',')
    |> Seq.map int
    |> Seq.toArray
let testInput = "3,4,3,1,2"

testInput
|> parseInput
|> run 80
|> printfn "%A"

readLines "input"
|> Seq.head
|> parseInput
|> run 80
|> printfn "%A"

testInput
|> parseInput
|> run 256
|> printfn "%A"

readLines "input"
|> Seq.head
|> parseInput
|> run 256
|> printfn "%A"

//           5_934
//         380_758
//   5_509_621_059
// 422_132_826_363

//  26_984_457_539
