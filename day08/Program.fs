open System.IO
open Microsoft.FSharp.Collections

let readLines (path: string) =
    seq {
        use reader = new StreamReader(path)
        while not reader.EndOfStream do
            yield reader.ReadLine()
    }

let digitsBySegmentCount =
    Map [ 2, 1; 4, 4; 3, 7; 7, 8 ]

let parseInput (input: string seq) =
    input
    |> Seq.map (fun line ->
        let [| signalsPart; outputsPart |] = line.Split(" | ")

        let signals =
            signalsPart.Split(" ")
            |> Array.toList
        let outputs =
            outputsPart.Split(" ")
            |> Array.toList

        signals, outputs)

let part1 (inputs: (string list * string list) seq) =
    inputs
    |> Seq.map snd
    |> Seq.map (fun outputs ->
        outputs
        |> Seq.choose (fun output -> Map.tryFind output.Length digitsBySegmentCount))
    |> Seq.concat
    |> Seq.length

let mapWithPossibleDigits (signals: string list) =
    let possibleDigitsBySegmentCount =
        Map [
            2, [ 1 ]
            3, [ 7 ]
            4, [ 4 ]
            5, [ 2; 3; 5 ]
            6, [ 0; 6; 9 ]
            7, [ 8 ]
        ]

    signals
    |> Seq.choose (fun signal ->
        Map.tryFind signal.Length possibleDigitsBySegmentCount
        |> Option.map (fun digit -> signal, digit))
    |> Seq.toList

// Segments
//  0000
// 1    2
// 1    2
//  3333
// 4    5
// 4    5
//  6666
let decode (signals: string list, outputs: string list) =
    let letters =
        ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g']
        |> Set

    let possibilities =
        letters
        |> List.replicate 7

    let segmentsForDigit (digit: int) =
        [ [ 0; 1; 2;    4; 5; 6 ]   // 0
          [       2;       5    ]   // 1
          [ 0;    2; 3; 4;    6 ]   // 2
          [ 0;    2; 3;    5; 6 ]   // 3
          [    1; 2; 3;    5;   ]   // 4
          [ 0; 1;    3;    5; 6 ]   // 5
          [ 0; 1;    3; 4; 5; 6 ]   // 6
          [ 0;    2;       5;   ]   // 7
          [ 0; 1; 2; 3; 4; 5; 6 ]   // 8
          [ 0; 1; 2; 3;    5; 6 ] ] // 9
        |> Seq.item digit

    let updatePossibilities (possibilities: char Set list) (signal: string) (segments: int list) =
        let signalsSet = Set signal

        possibilities
        |> List.mapi (fun index possibleSignals ->
            if Seq.contains index segments then
                Set.intersect possibleSignals signalsSet
            else
                Set.difference possibleSignals signalsSet)

    let hasInvalidAssignment (possibilities: char Set list) =
        possibilities
        |> List.exists Set.isEmpty

    let rec decodeSignalAssignment
        (possibilities: char Set list, digits: (string * int) list)
        (remaining: (string * int list) list) : (string * int) list =

        match remaining with
        | [] -> digits
        | (signal, [ candidate ]) :: rest ->
            let segments = segmentsForDigit candidate
            let updatedPossibilities = updatePossibilities possibilities signal segments
            let updatedDigits = (signal, candidate) :: digits

            decodeSignalAssignment (updatedPossibilities, updatedDigits) rest
        | (signal, candidates) :: rest ->
            let possibleCandidates =
                candidates
                |> List.where (fun candidate ->
                    let segments = segmentsForDigit candidate
                    let updatedPossibilities = updatePossibilities possibilities signal segments
                    not (hasInvalidAssignment updatedPossibilities))

            let newRemaining = List.append rest [(signal, possibleCandidates)]
            decodeSignalAssignment (possibilities, digits) newRemaining

    let digitsMap =
        signals
        |> mapWithPossibleDigits
        |> decodeSignalAssignment (possibilities, [])

    outputs
    |> Seq.toList
    |> List.choose (fun output ->
        digitsMap
        |> Seq.tryPick (fun (signal, digit) ->
            if Set signal = Set output then
                Some digit
            else
                None))
    |> List.rev
    |> List.mapi (fun index digit -> (pown 10 index) * digit)
    |> List.sum

let testInput =
   [ "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
     "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
     "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
     "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
     "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
     "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
     "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
     "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
     "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
     "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce" ]


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
|> Seq.map decode
|> Seq.sum
|> printfn "%A"

readLines "input"
|> parseInput
|> Seq.map decode
|> Seq.sum
|> printfn "%A"
