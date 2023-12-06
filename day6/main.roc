app "AoC"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }

    imports [pf.Stdout, pf.Task.{ Task }, "input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    p1 = (part1 input)
    p2 = (part2 input)

    Stdout.line "P1: \(p1 |> Num.toStr) P2: \(p2 |> Num.toStr)"

part1 = \str ->
    { before, after } = 
        Str.splitFirst str "\n" 
        |> unwrap
    times = before |> numbers
    distances = after |> numbers
    races = List.map2 times distances Race
    List.map races countWinningTimes
        |> List.product

part2 = \str ->
    { before, after } = 
        Str.splitFirst str "\n" 
        |> unwrap
    time = before |> number
    distance = after |> number
    countWinningTimes (Race time distance)

countWinningTimes = \ (Race time distance) ->
    lower = time / 2 - Num.sqrt (time*time / 4 - distance) + 1 |> Num.floor
    upper = time / 2 + Num.sqrt (time*time / 4 - distance) - 1 |> Num.ceiling
    upper - lower + 1
    

number = \str ->
    Str.splitFirst str ": "
    |> unwrap
    |> .after
    |> Str.replaceEach " " ""
    |> Str.toF64
    |> unwrap

numbers = \str ->
    Str.splitFirst str ": "
    |> unwrap
    |> .after
    |> Str.split " "
    |> List.keepOks Str.toF64

example : Str
example =
    """
    Time:      7  15   30
    Distance:  9  40  200
    """

unwrap = \res ->
    when res is
        Ok x -> x
        Err _ -> crash "bad unwrap"
