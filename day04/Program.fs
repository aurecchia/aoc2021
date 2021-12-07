open System
open System.IO

let readLines (path: string) =
    seq {
        use reader = new StreamReader(path)
        while not reader.EndOfStream do
            yield reader.ReadLine()
    }

type Board = {
    Numbers: int list
    Marked: bool array
}

module Board =
    let size = 5
    let create (numbers: int list) =
        { Numbers = numbers
          Marked = Array.replicate numbers.Length false }

    let tryFindNumberPosition (board: Board) (number: int) =
        board.Numbers
        |> List.tryFindIndex (fun n -> n = number)
        |> Option.map (fun index ->
            let row = index / size
            let column = index % size

            row, column)

    let markPosition (board: Board) (row: int, column: int) =
        let index = (row * size) + column

        let marked = Array.copy(board.Marked)
        Array.set marked index true

        { board with Marked = marked }

    let markNumber (number: int) (board: Board) =
        tryFindNumberPosition board number
        |> Option.map (markPosition board)
        |> Option.defaultValue board

    let hasCompleteRow (board: Board) =
        board.Marked
        |> Array.chunkBySize size
        |> Array.exists (Array.forall id)

    let hasCompleteColumn (board: Board) =
        board.Marked
        |> Array.chunkBySize size
        |> Array.transpose
        |> Array.exists (Array.forall id)

    let isComplete (board: Board) =
        hasCompleteRow board || hasCompleteColumn board

    let getUnmarkedNumbers (board: Board) : int seq =
        board.Numbers
        |> Seq.zip board.Marked
        |> Seq.where (fst >> not)
        |> Seq.map snd

    let print (board: Board) =
        board.Numbers
        |> Seq.zip board.Marked
        |> Seq.map (fun (marked, number) ->
            if marked then
                sprintf "(%2d)" number
            else
                sprintf " %2d " number)
        |> Seq.chunkBySize 5
        |> Seq.map (String.concat " ")
        |> Seq.iter (printfn "%s")

let parseInput (lines: string seq) : int seq * Board seq =
    let numbers =
        lines
        |> Seq.head
        |> fun s -> s.Split(',')
        |> Seq.map int

    let parseBoardLine (line: string) =
        line.Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray
        |> List.map int

    let parseBoard boardLines =
        boardLines
        |> Seq.map parseBoardLine
        |> List.concat
        |> Board.create

    let boards =
        lines
        |> Seq.skip 1
        |> Seq.chunkBySize 6
        |> Seq.map (fun boardLines ->
            boardLines
            |> Seq.skip 1
            |> parseBoard)

    numbers, boards


let runGame (numbers: int seq) (boards: Board seq): Board * int =

    let applyNumber (number: int) (boards: Board seq): (Board * int) option * Board seq =
        let updatedBoards =
            boards
            |> Seq.map (Board.markNumber number)

        let winner =
            updatedBoards
            |> Seq.tryFind Board.isComplete
            |> Option.map (fun board -> board, number)

        (winner, updatedBoards)

    let folder (finalState: (Board * int) option, boards: Board seq) (number: int) =
        match finalState with
        | Some winner -> Some winner, boards
        | None -> applyNumber number boards

    numbers
    |> Seq.fold folder (None, boards)
    |> fst
    |> Option.get

let runGame2 (numbers: int seq) (boards: Board seq): Board * int =

    let applyNumber (number: int) (boards: Board seq): (Board * int) option * Board seq =
        let updatedBoards =
            boards
            |> Seq.map (Board.markNumber number)

        let winner =
            updatedBoards
            |> Seq.tryFind Board.isComplete
            |> Option.map (fun board -> board, number)

        (winner, updatedBoards)

    let folder (currentWinner: (Board * int) option, boards: Board seq) (number: int) =
        let newWinner, updatedBoards = applyNumber number boards

        let winner =
            newWinner
            |> Option.orElse currentWinner

        let newBoards =
            updatedBoards
            |> Seq.where (Board.isComplete >> not)

        winner, newBoards

    numbers
    |> Seq.fold folder (None, boards)
    |> fst
    |> Option.get

let run input =
    let numbers, boards =
        input
        |> parseInput

    let board, number = runGame numbers boards

    Board.print board
    printfn "%d" number

    let unmarkedSum =
        Board.getUnmarkedNumbers board
        |> Seq.sum

    printfn "%d" (unmarkedSum * number)

let run2 input =
    let numbers, boards =
        input
        |> parseInput

    let board, number = runGame2 numbers boards

    Board.print board
    printfn "%d" number

    let unmarkedSum =
        Board.getUnmarkedNumbers board
        |> Seq.sum

    printfn "%d" (unmarkedSum * number)

let testInput = """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"""

testInput.Split("\n") |> run
printfn ""
readLines "input" |> run
printfn ""
readLines "input" |> run2
