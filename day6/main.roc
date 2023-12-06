app "AoC"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }

    imports [pf.Stdout, pf.Task.{ Task }, "input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    dbg
        P1 (part1 example)

    dbg
        P2 (part2 example)

    Stdout.line "done"

part1 = \str ->
    { before, after } = 
        Str.splitFirst str "\n" 
        |> unwrap
    times = before |> numbers
    distances = after |> numbers
    races = List.map2 times distances Race
    List.map races countWinningTimes

countWinningTimes = \ (Race time distance) ->
    lower = time / 2 - Num.sqrt (time*time / 4 - distance)
    upper = time / 2 + Num.sqrt (time*time / 4 - distance)
    Num.round 34.32f32
    

numbers = \str ->
    Str.splitFirst str ": "
    |> unwrap
    |> .after
    |> Str.split " "
    |> List.keepOks Str.toF64

part2 = \str ->
    X
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
