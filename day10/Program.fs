open System.IO

let readLines (path: string) =
    seq {
        use reader = new StreamReader(path)
        while not reader.EndOfStream do
            yield reader.ReadLine()
    }

let scopeCharacters =
    Map [ ('(', ')'); ('[', ']'); ('{', '}'); ('<', '>') ]

let isOpenChar (character: char) =
    Map.containsKey character scopeCharacters

let closingCharacterFor (openScopeChar: char) =
    Map.find openScopeChar scopeCharacters

let isMatchingCloseChar (openScopeChar: char) (closeScopeChar: char) =
    closingCharacterFor openScopeChar = closeScopeChar

let findScopingErrors (error: char option, openScopes: char list) (character: char) =
    match error, openScopes with
    | Some _, _ -> error, openScopes
    | None, openScopeChar :: rest ->
        if isOpenChar character then
            (error, character :: openScopes)
        elif isMatchingCloseChar openScopeChar character then
            (error, rest)
        else
            (Some character, openScopes)
    | None, _ ->
        if isOpenChar character then
            (error, character :: openScopes)
        else
            (Some character, openScopes)

let pointsPerError (error: char) =
    match error with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137

let part1 (input: char array seq) =
    input
    |> Seq.map (fun line ->
        line
        |> Seq.fold findScopingErrors (None, []))
    |> Seq.choose fst
    |> Seq.map pointsPerError
    |> Seq.sum

let completeOpenScope (openScope: char list) : char list =
    openScope
    |> List.map closingCharacterFor

let pointsForClosingChar (closingChar: char) =
    match closingChar with
    | ')' -> 1
    | ']' -> 2
    | '}' -> 3
    | '>' -> 4

let correctionScore (closingCharacters: char list) =
    closingCharacters
    |> List.map (pointsForClosingChar >> int64)
    |> List.fold (fun score pointPerCorrection -> score * 5L + pointPerCorrection) 0L

let part2 (input: char array seq) =
    input
    |> Seq.map (fun line ->
        line
        |> Seq.fold findScopingErrors (None, []))
    |> Seq.choose
        (function
         | None, openScopes when not (List.isEmpty openScopes) -> Some openScopes
         | _ -> None)
    |> Seq.map completeOpenScope
    |> Seq.map correctionScore
    |> Seq.sort
    |> (fun scores -> Seq.item (Seq.length scores / 2) scores)

let parseInput (input: string seq) =
    input
    |> Seq.map (fun line -> line.ToCharArray())

let testInput =
    [ "[({(<(())[]>[[{[]{<()<>>"
      "[(()[<>])]({[<{<<[]>>("
      "{([(<{}[<>[]}>{[]{[(<()>"
      "(((({<>}<{<{<>}{[]{[]{}"
      "[[<[([]))<([[{}[[()]]]"
      "[{[{({}]{}}([{[{{{}}([]"
      "{<[[]]>}<{[{[{[]{()[[[]"
      "[<(<(<(<{}))><([]([]()"
      "<{([([[(<>()){}]>(<<{{"
      "<{([{{}}[<[[[<>{}]]]>[]]" ]

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
