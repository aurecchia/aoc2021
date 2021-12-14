open System
open System.IO

let readLines (path: string) =
    seq {
        use reader = new StreamReader(path)
        while not reader.EndOfStream do
            yield reader.ReadLine()
    }

let parseInput (input: string seq) =
    input
    |> Seq.map (fun line ->
        line.Split('-')
        |> Seq.pairwise
        |> Seq.head)

type Room = string

module Room =
    let isStart (room: Room) =
        room = "start"
    let isEnd (room: Room) =
        room = "end"
    let isSmall (room: Room) =
        room |> Seq.forall Char.IsLower

let updateGraph (graph: Map<Room, Room Set>) (entry, exit: Room) =

    let updateReachable (exit: Room) (reachable: Room Set option) =
        match reachable with
        | Some exits -> Some (Set.add exit exits)
        | None -> Some (Set.singleton exit)

    let addEdge (a, b: Room) (graph: Map<Room, Room Set>) =
        if Room.isEnd a || Room.isStart b then
            graph
        else
            Map.change a (updateReachable b) graph
    graph
    |> addEdge (entry, exit)
    |> addEdge (exit, entry)


let rec findPathFrom (graph: Map<Room, Room Set>, visited: Room Set, smallVisitsAllowed: int) (node: Room) : Room list
 list =
    if Room.isEnd node then
        [ [] ]
    else
        let reachable = Map.find node graph
        let visitable =
            if smallVisitsAllowed > 0 then
                reachable
            else
                Set.difference reachable visited

        visitable
        |> Set.toList
        |> List.collect (fun next ->
            let paths =
                let newSmallVisitsAllowed =
                    if Set.contains next visited then
                        smallVisitsAllowed - 1
                    else
                        smallVisitsAllowed

                let newVisited =
                    if Room.isSmall next then
                        Set.add next visited
                    else
                        visited

                findPathFrom (graph, newVisited, newSmallVisitsAllowed) next

            let newPaths =
                paths
                |> List.map (fun path -> next :: path)

            newPaths)

let findAllPathsWithNoAdditionalVisits (graph: Map<Room, Room Set>) =
    findPathFrom (graph, Set.singleton "start", 0) "start"
    |> List.map (fun path -> "start" :: path)

let findAllPathsWithOneAdditionalVisit (graph: Map<Room, Room Set>) =
    findPathFrom (graph, Set.singleton "start", 1) "start"
    |> List.map (fun path -> "start" :: path)

let testInput1 =
    [ "start-A"
      "start-b"
      "A-c"
      "A-b"
      "b-d"
      "A-end"
      "b-end" ]

let testInput2 =
    [ "dc-end"
      "HN-start"
      "start-kj"
      "dc-start"
      "dc-HN"
      "LN-dc"
      "HN-end"
      "kj-sa"
      "kj-HN"
      "kj-dc" ]

let testInput3 =
    [ "fs-end"
      "he-DX"
      "fs-he"
      "start-DX"
      "pj-DX"
      "end-zg"
      "zg-sl"
      "zg-pj"
      "pj-he"
      "RW-he"
      "fs-DX"
      "pj-RW"
      "zg-RW"
      "start-pj"
      "he-WI"
      "zg-he"
      "pj-fs"
      "start-RW" ]

testInput2
|> parseInput
|> Seq.fold updateGraph Map.empty
|> findAllPathsWithNoAdditionalVisits
|> List.length
|> (printfn "%A")

testInput3
|> parseInput
|> Seq.fold updateGraph Map.empty
|> findAllPathsWithNoAdditionalVisits
|> List.length
|> (printfn "%A")

readLines "input"
|> parseInput
|> Seq.fold updateGraph Map.empty
|> findAllPathsWithNoAdditionalVisits
|> List.length
|> (printfn "%A")

testInput2
|> parseInput
|> Seq.fold updateGraph Map.empty
|> findAllPathsWithOneAdditionalVisit
|> List.length
|> printfn "%A"

testInput3
|> parseInput
|> Seq.fold updateGraph Map.empty
|> findAllPathsWithOneAdditionalVisit
|> List.length
|> (printfn "%A")

readLines "input"
|> parseInput
|> Seq.fold updateGraph Map.empty
|> findAllPathsWithOneAdditionalVisit
|> List.length
|> (printfn "%A")
