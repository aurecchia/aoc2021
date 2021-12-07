open System
open System.IO

let readLines (path: string) =
    seq {
        use reader = new StreamReader(path)
        while not reader.EndOfStream do
            yield reader.ReadLine()
    }

let digitCount = snd
let takeDigit = fst


let binaryDigitsToInt digits =
    digits
    |> Seq.toArray
    |> String
    |> fun binary -> Convert.ToInt32(binary, 2)

let computeConsumptionRate (input: string seq) =
    let digitCount = snd
    let takeDigit = fst

    let gammaDigits =
        input
        |> Seq.transpose
        |> Seq.map (Seq.countBy id)
        |> Seq.map (Seq.maxBy digitCount)
        |> Seq.map takeDigit

    let epsilonDigits =
        gammaDigits
        |> Seq.map (fun digit -> if digit = '0' then '1' else '0')

    let gammaRate = binaryDigitsToInt gammaDigits
    let epsilonRate = binaryDigitsToInt epsilonDigits

    gammaRate * epsilonRate

let rec co2ScrubberRating (state: int * string seq) =
    let index, numbers = state

    if Seq.length numbers = 1 then
        Seq.head numbers
        |> fun binary -> Convert.ToInt32(binary, 2)
    else
        let digits =
            numbers
            |> Seq.transpose
            |> Seq.skip index
            |> Seq.head

        let common =
            digits
            |> Seq.countBy id
            |> Seq.groupBy digitCount
            |> Seq.minBy fst
            |> snd
            |> Seq.map takeDigit
            |> fun digits -> if Seq.length digits > 1 then '0' else Seq.head digits

        let remaining =
            numbers
            |> Seq.where (fun number -> number[index] = common)

        co2ScrubberRating (index + 1, remaining)

let rec oxygenGeneratorRating (state: int * string seq) =
    let index, numbers = state

    if Seq.length numbers = 1 then
        Seq.head numbers
        |> fun binary -> Convert.ToInt32(binary, 2)
    else
        let digits =
            numbers
            |> Seq.transpose
            |> Seq.skip index
            |> Seq.head

        let common =
            digits
            |> Seq.countBy id
            |> Seq.groupBy digitCount
            |> Seq.maxBy fst
            |> snd
            |> Seq.map takeDigit
            |> fun digits -> if Seq.length digits > 1 then '1' else Seq.head digits

        let remaining =
            numbers
            |> Seq.where (fun number -> number[index] = common)

        oxygenGeneratorRating (index + 1, remaining)

let part2 (input: string seq) =

    let oxygen =
        oxygenGeneratorRating (0, input)

    let co2 =
        co2ScrubberRating (0, input)

    oxygen * co2

let testInput = [
    "00100"
    "11110"
    "10110"
    "10111"
    "10101"
    "01111"
    "00111"
    "11100"
    "10000"
    "11001"
    "00010"
    "01010"
]

let input =
    readLines "input"

testInput
|> computeConsumptionRate
|> printfn "%A"

input
|> computeConsumptionRate
|> printfn "%A"

testInput
|> part2
|> printfn "%A"

input
|> part2
|> printfn "%A"
