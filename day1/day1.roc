app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
    }
    imports [pf.Stdout, pf.Task.{ Task }, "day-1-input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    part1 =
        input
        |> calibrationValues
        |> List.sum
        |> Num.toStr

    part2 =
        input
        |> addNumsToNumstring
        |> calibrationValues
        |> List.sum
        |> Num.toStr

    Stdout.line "Part 1: \(part1) \nPart 2: \(part2)"

calibrationValues = \inp ->
    line <- lines inp |> List.map
    first = List.first line
    last = List.last line

    when (first, last) is
        (Ok frst, Ok lst) ->
            num = (frst - '0') * 10 + lst - '0'
            Num.toU64 num

        _ -> crash "invalid input: line without numbers"

addNumsToNumstring = \str ->
    str
    |> Str.replaceEach "one" "one1one"
    |> Str.replaceEach "two" "two2two"
    |> Str.replaceEach "three" "three3three"
    |> Str.replaceEach "four" "four4four"
    |> Str.replaceEach "five" "five5five"
    |> Str.replaceEach "six" "six6six"
    |> Str.replaceEach "seven" "seven7seven"
    |> Str.replaceEach "eight" "eight8eight"
    |> Str.replaceEach "nine" "nine9nine"

lines : Str -> List (List U8)
lines = \str ->
    line <- str
        |> Str.split "\n"
        |> List.map Str.toUtf8
        |> List.map

    List.keepIf line isDigit

isDigit : U8 -> Bool
isDigit = \b ->
    b >= '0' && b <= '9'

expect lines example == [['1', '2'], ['3', '8'], ['1', '2', '3', '4', '5'], ['7']]
expect calibrationValues example == [12, 38, 15, 77]

example : Str
example =
    """
    1abc2
    pqr3stu8vwx
    a1b2c3d4e5f
    treb7uchet
    """
