
open System
open System.IO

let readLines (path: string) =
    seq {
        use reader = new StreamReader(path)
        while not reader.EndOfStream do
            yield reader.ReadLine()
    }

type Coord = int * int
type VentLine = Coord * Coord

let parseCoordinate (input: string) : Coord =
    input.Split(',')
    |> Seq.map int
    |> Seq.pairwise
    |> Seq.head

let parseLine (line: string) : VentLine =
    line.Split(" -> ")
    |> Seq.map parseCoordinate
    |> Seq.pairwise
    |> Seq.head

let updateOceanFloor (oceanFloor: Map<Coord, int>) (coord: Coord) =
    let newOrIncrease =
       function
       | Some old -> Some (old + 1)
       | None -> Some 1

    oceanFloor
    |> Map.change coord newOrIncrease

let applyVent (oceanFloor: Map<Coord, int>) (ventCoordinates: VentLine) =
    let (x1, y1), (x2, y2) = ventCoordinates

    if x1 = x2 then
        seq { for y in (min y1 y2) .. (max y1 y2) -> (x1, y) }
        |> Seq.fold updateOceanFloor oceanFloor
    elif y1 = y2 then
        seq { for x in (min x1 x2) .. (max x1 x2) -> (x, y1) }
        |> Seq.fold updateOceanFloor oceanFloor
    else
        oceanFloor

let applyVentWithDiagonals (oceanFloor: Map<Coord, int>) (ventCoordinates: VentLine) =
    let (x1, y1), (x2, y2) = ventCoordinates

    let (..) a b =
        if a < b then seq { a .. b }
                 else seq { a .. -1 .. b }

    if x1 = x2 then
        seq { for y in y1 .. y2 -> (x1, y) }
        |> Seq.fold updateOceanFloor oceanFloor
    elif y1 = y2 then
        seq { for x in x1 .. x2 -> (x, y1) }
        |> Seq.fold updateOceanFloor oceanFloor
    else
        Seq.zip (seq { for x in x1 .. x2 -> x }) (seq { for y in y1 .. y2 -> y })
        |> Seq.fold updateOceanFloor oceanFloor

let run1 (ventLines: VentLine seq) =
    ventLines
    |> Seq.fold applyVent Map.empty
    |> Map.values
    |> Seq.where (fun v -> v >= 2)
    |> Seq.length

let run2 (ventLines: VentLine seq) =
    ventLines
    |> Seq.fold applyVentWithDiagonals Map.empty
    |> Map.values
    |> Seq.where (fun v -> v >= 2)
    |> Seq.length

let testInput =
    [ "0,9 -> 5,9"
      "8,0 -> 0,8"
      "9,4 -> 3,4"
      "2,2 -> 2,1"
      "7,0 -> 7,4"
      "6,4 -> 2,0"
      "0,9 -> 2,9"
      "3,4 -> 1,4"
      "0,0 -> 8,8"
      "5,5 -> 8,2" ]

testInput
|> Seq.map parseLine
|> run1
|> printfn "%A"

readLines "input"
|> Seq.map parseLine
|> run1
|> printfn "%A"

testInput
|> Seq.map parseLine
|> run2
|> printfn "%A"

readLines "input"
|> Seq.map parseLine
|> run2
|> printfn "%A"
