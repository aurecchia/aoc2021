open System.IO

let readLines (path: string) =
    seq {
        use reader = new StreamReader(path)
        while not reader.EndOfStream do
            yield reader.ReadLine()
    }

let adjacentLocations (x, y: int) (width, height: int) : (int * int) seq =
    [ (-1, 0); (1, 0); (0, -1); (0, 1) ]
    |> Seq.map (fun (xDiff, yDiff) -> (x + xDiff, y + yDiff))
    |> Seq.choose (fun (x, y) ->
        if x < 0 || x >= width || y < 0 || y >= height then
            None
        else Some (x, y))

let getHeightAt (heightMap: int array array) (x, y: int) =
    Array.get heightMap x
    |> fun row -> Array.get row y

let findLowPoints (heightMap: int array array) =
    let dimensions = (Array.length heightMap, Array.head heightMap |> Array.length)

    let mapStuff x y height =
        let adjacent = adjacentLocations (x, y) dimensions
        let isLowPoint =
            adjacent
            |> Seq.map (getHeightAt heightMap)
            |> Seq.forall (fun h -> h > height)

        if isLowPoint then
            height + 1
        else
            0

    heightMap
    |> Array.mapi (fun x row ->
        row
        |> Array.mapi (mapStuff x)
        |> Array.sum)
    |> Array.sum

let isInBasin (heightMap: int array array) (height: int) (coord: int * int) =
    let heightAtCoord = getHeightAt heightMap coord
    heightAtCoord < 9 && heightAtCoord >= height

let rec findBasinSize (basin: Set<int * int>) (heightMap: int array array) (coords: (int * int) list) : int =
    match coords with
    | [] -> basin.Count
    | coord :: rest ->
        let dimensions = (Array.length heightMap, Array.head heightMap |> Array.length)
        let height = getHeightAt heightMap coord

        let remainingAdjacentInBasin =
            adjacentLocations coord dimensions
            |> Seq.where (isInBasin heightMap height)
            |> Seq.where (fun coord -> not (Set.contains coord basin))
            |> Seq.toList

        let remainingCoords = remainingAdjacentInBasin @ rest

        findBasinSize (Set.add coord basin) heightMap remainingCoords

let findBasins (heightMap: int array array) =
    let dimensions = (Array.length heightMap, Array.head heightMap |> Array.length)

    let mapStuff x y height =
        let adjacent = adjacentLocations (x, y) dimensions
        let isLowPoint =
            adjacent
            |> Seq.map (getHeightAt heightMap)
            |> Seq.forall (fun h -> h > height)

        if isLowPoint then
            findBasinSize Set.empty heightMap [(x, y)]
        else
            0

    heightMap
    |> Array.mapi (fun x row ->
        row
        |> Array.mapi (mapStuff x)
        |> Array.where (fun size -> size > 0))
    |> Array.concat
    |> Array.sortDescending
    |> Array.take 3
    |> Array.reduce (*)

let parseInput (input: string seq) : int array array =
    input
    |> Seq.map (fun s ->
        s
        |> Seq.map (string >> int)
        |> Seq.toArray)
    |> Seq.toArray

let testInput = [
    "2199943210"
    "3987894921"
    "9856789892"
    "8767896789"
    "9899965678"
]

testInput
|> parseInput
|> findLowPoints
|> printfn "%A"

readLines "input"
|> parseInput
|> findLowPoints
|> printfn "%A"

testInput
|> parseInput
|> findBasins
|> printfn "%A"

readLines "input"
|> parseInput
|> findBasins
|> printfn "%A"
