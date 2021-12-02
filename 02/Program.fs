open System.IO

let readLines (path: string) =
    seq {
        use reader = new StreamReader (path)
        while not reader.EndOfStream do
            yield reader.ReadLine ()
    }

type Command =
    | Forward of distance: int
    | Down of depth: int
    | Up of depth: int

    static member fromString (input: string) =
        match input.Split(' ') with
        | [| "forward"; value |] -> Forward (int value)
        | [| "down"; value |] -> Down (int value)
        | [| "up"; value |] -> Up (int value)
        | other -> failwithf $"BLOW UP! ({other})"

 type Position =
     | Position of distance: int * depth: int

     static member initial =  Position (0, 0)

     static member applyCommand (Position (h, depth)) =
         function
         | Forward distance -> Position (h + distance, depth)
         | Down down -> Position (h, depth + down)
         | Up up -> Position (h, depth - up)

type PositionWithAim =
     | PositionWithAim of horizontal: int * depth: int * aim: int

     static member initial =  PositionWithAim (0, 0, 0)

     static member applyCommand (PositionWithAim (distance, depth, aim)) =
         function
         | Forward units ->
             PositionWithAim (distance + units, depth + aim * units, aim)
         | Down units ->
             PositionWithAim (distance, depth, aim + units)
         | Up units ->
             PositionWithAim (distance, depth, aim - units)

let testCommands =
    [ "forward 5"
      "down 5"
      "forward 8"
      "up 3"
      "down 8"
      "forward 2" ]
    |> Seq.ofList
    |> Seq.map Command.fromString

let commands =
    readLines "input"
    |> Seq.map Command.fromString

// PART 1
testCommands
|> Seq.fold Position.applyCommand Position.initial
|> fun (Position (h, d)) -> printfn "%d" (h * d)

commands
|> Seq.fold Position.applyCommand Position.initial
|> fun (Position (h, d)) -> printfn "%d" (h * d)

// PART 2
testCommands
|> Seq.fold PositionWithAim.applyCommand PositionWithAim.initial
|> fun (PositionWithAim (h, d, _)) -> printfn "%d" (h * d)

commands
|> Seq.fold PositionWithAim.applyCommand PositionWithAim.initial
|> fun (PositionWithAim (h, d, _)) -> printfn "%d" (h * d)
