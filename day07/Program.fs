open System.IO

let readLines (path: string) =
    seq {
        use reader = new StreamReader(path)
        while not reader.EndOfStream do
            yield reader.ReadLine()
    }

let linearMovementCost (endPosition: int) (startPosition: int) =
    if startPosition >= endPosition then
        startPosition - endPosition
    else
        endPosition - startPosition

let exponentialMovementCost (endPosition: int) (startPosition: int) =
    let cost = (linearMovementCost endPosition startPosition)

    cost * (cost + 1) / 2


let alignmentCost (costFunction: int -> int -> int) (crabs: int seq) (endPosition: int) =
    crabs
    |> Seq.sumBy (costFunction endPosition)

let bestAlignment (costFunction: int -> int -> int) (crabs: int seq) =
    let minPos = Seq.min crabs
    let maxPos = Seq.max crabs

    let bestPosition =
        seq { for pos in minPos .. maxPos -> pos }
        |> Seq.minBy (alignmentCost costFunction crabs)

    alignmentCost costFunction crabs bestPosition

let parseInput (input: string seq) =
    input
    |> Seq.head
    |> fun s -> s.Split(',')
    |> Seq.map int

let testInput =
    "16,1,2,0,4,2,7,1,2,14"
    |> Seq.singleton

testInput
|> parseInput
|> bestAlignment linearMovementCost
|> printfn "%A"

readLines "input"
|> parseInput
|> bestAlignment linearMovementCost
|> printfn "%A"

testInput
|> parseInput
|> bestAlignment exponentialMovementCost
|> printfn "%A"

readLines "input"
|> parseInput
|> bestAlignment exponentialMovementCost
|> printfn "%A"
