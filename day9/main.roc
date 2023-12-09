app "AoC"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }

    imports [pf.Stdout, pf.Task.{ Task }, "input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    dbg
        part1 input |> List.sum

    dbg
        part2 input |> List.sum

    Stdout.line "Ok"

part1 = \str ->
    line <- Str.split str "\n" |> List.map
    Str.split line " "
    |> List.keepOks Str.toI64
    |> getNextNumber

getNextNumber = \list ->
    if List.all list \x -> x == 0 then
        0
    else
        nextLine = getNextLine list []
        (List.last list |> unwrap) + (List.last nextLine |> unwrap)

getNextLine = \list, nextLine ->
    when list is
        [a, b, ..] -> getNextLine (List.dropAt list 0) (List.append nextLine (b - a))
        [a] -> List.append nextLine (getNextNumber nextLine) |> List.prepend (getPreviousNumber nextLine)
        [] -> nextLine

part2 = \str ->
    line <- Str.split str "\n" |> List.map
    Str.split line " "
    |> List.keepOks Str.toI64
    |> getPreviousNumber

getPreviousNumber = \list ->
    if List.all list \x -> x == 0 then
        0
    else
        nextLine = getNextLine list []
        (List.first list |> unwrap) - (List.first nextLine |> unwrap)

example : Str
example =
    """
    0 3 6 9 12 15
    1 3 6 10 15 21
    10 13 16 21 30 45
    """

unwrap = \res ->
    when res is
        Ok x -> x
        Err _ ->
            crash "bad unwrap"

debug = \x ->
    dbg
        x

    x
