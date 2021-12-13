open System.IO

let readLines (path: string) =
    seq {
        use reader = new StreamReader(path)
        while not reader.EndOfStream do
            yield reader.ReadLine()
    }

type Board = int array array

module Board =
    let size (board: Board) =
        Array.length board * (board |> Array.head |> Array.length)

    let adjacentTo (board: Board) (x, y: int) =
        let width = Array.length board
        let height = Array.length (Array.head board)

        [ (x - 1, y - 1); (x, y - 1); (x + 1, y - 1)
          (x - 1, y    );             (x + 1, y    )
          (x - 1, y + 1); (x, y + 1); (x + 1, y + 1) ]
        |> Seq.where (fun (x, y) ->
            (x >= 0 && x < width) && (y >= 0 && y < height))

    let getOctopusAt (board: Board) (x, y: int) =
        Array.get board x
        |> (fun row -> Array.get row y)

    let willFlash (octopus: int) = octopus > 9

    let willFlashAt (board: Board) (x, y: int) =
        getOctopusAt board (x, y)
        |> willFlash

    let numberOfAdjacentFlashesTo (board: Board) (x, y: int) =
        adjacentTo board (x, y)
        |> Seq.where (willFlashAt board)
        |> Seq.length

    let coordinates (board: Board) =
        Seq.allPairs
            [ 0 .. (board.Length - 1) ]
            [ 0 .. (board |> Array.head |> Array.length) - 1 ]

    let set (octopus: int) (board: Board) (x, y: int) : Board =
        let newRow =
            Array.get board x
            |> Array.updateAt y octopus

        board
        |> Array.updateAt x newRow

    let anyWillFlash (board: Board) =
        coordinates board
        |> Seq.exists (willFlashAt board)

    let step (board: Board) (x, y: int) : Board =
        let octopus = getOctopusAt board (x, y)
        set (octopus + 1) board (x, y)

    let inc (board: Board) (x, y: int) : Board =
        let octopus = getOctopusAt board (x, y)
        if octopus = 0 then
            board
        else
            set (octopus + 1) board (x, y)

    let map (f: int -> int) (board: Board) =
        board
        |> Array.map (fun row ->
            row
            |> Array.map (fun octopus -> f octopus))

let rec propagateFlashes (board: Board, flashes: int) : Board * int =
    let flashingOctopuses =
        Board.coordinates board
        |> Seq.where (Board.willFlashAt board)

    let propagations =
        flashingOctopuses
        |> Seq.collect (Board.adjacentTo board)

    let flashedBoard =
        flashingOctopuses
        |> Seq.fold (Board.set 0) board

    let updatedBoard =
        propagations
        |> Seq.fold Board.inc flashedBoard

    let newFlashes = flashes + Seq.length flashingOctopuses
    if Board.anyWillFlash board then
        propagateFlashes (updatedBoard, newFlashes)
    else
        updatedBoard, newFlashes

let runStep (board: Board, flashes: int) (step: int) : Board * int =
    let increasedBoard =
        Board.coordinates board
        |> Seq.fold Board.step board

    propagateFlashes (increasedBoard, flashes)

let runStep2 (board: Board, firstCoordination: int option) (step: int) : Board * (int option) =
    match firstCoordination with
    | Some _ -> (board, firstCoordination)
    | None ->
        let increasedBoard =
            Board.coordinates board
            |> Seq.fold Board.step board

        let newBoard, newFlashes = propagateFlashes (increasedBoard, 0)
        if newFlashes = Board.size board then
            newBoard, Some step
        else
            newBoard, None


let part1 (board: int array array) =
    [ for i in 0 .. 99 -> i ]
    |> Seq.fold runStep (board, 0)
    |> snd

let part2 (board: int array array) =
    [ for i in 0 .. 1000 -> i ]
    |> Seq.fold runStep2 (board, None)
    |> snd

let parseInput (input: string seq) =
    input
    |> Seq.map (fun line ->
        line.ToCharArray()
        |> Array.map (string >> int))
    |> Seq.toArray

let testInput =
    [ "5483143223"
      "2745854711"
      "5264556173"
      "6141336146"
      "6357385478"
      "4167524645"
      "2176841721"
      "6882881134"
      "4846848554"
      "5283751526" ]

testInput
|> parseInput
|> part1
|> printfn "%A"

readLines "input"
|> parseInput
|> part1
|> printfn "%A"

testInput
|> parseInput
|> part2
|> printfn "%A"

readLines "input"
|> parseInput
|> part2
|> printfn "%A"
