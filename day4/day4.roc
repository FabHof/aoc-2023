app "AoC"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }

    imports [pf.Stdout, pf.Task.{ Task }, "day-4-input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    dbg
        P1 (part1 input)

    dbg
        P2 (part2 input)

    Stdout.line "done"

part1 = \str ->
    points =
        line <- str |> Str.split "\n" |> List.map
        { before, after } =
            line
            |> Str.splitFirst ": "
            |> unwrap
            |> .after
            |> Str.splitFirst " | "
            |> unwrap
        winning = Str.split before " " |> List.dropIf Str.isEmpty
        youHave = Str.split after " " |> List.dropIf Str.isEmpty
        count =
            youHave
            |> List.keepIf \n -> List.contains winning n
            |> List.len
        if count > 0 then
            Num.powInt 2 (count - 1)
        else
            0
    List.sum points

part2 = \str ->
    cards =
        line <- str |> Str.split "\n" |> List.map
        { before, after } =
            line
            |> Str.splitFirst ": "
            |> unwrap
            |> .after
            |> Str.splitFirst " | "
            |> unwrap
        winning = Str.split before " " |> List.dropIf Str.isEmpty
        youHave = Str.split after " " |> List.dropIf Str.isEmpty
        wins =
            youHave
            |> List.keepIf \n -> List.contains winning n
            |> List.len
        { count: 1, wins: wins }
    List.walkWithIndex cards cards addMoreCards
    |> List.map .count
    |> List.sum

addMoreCards = \state, elem, index ->
    addWins state (List.get state index |> unwrap |> .count) elem.wins index

addWins = \state, count, wins, index ->
    if wins > 0 then
        nextIndex = index + 1
        when List.get state nextIndex is
            Ok item ->
                newState = List.set state nextIndex ({ item & count: item.count + count })
                addWins newState count (wins - 1) nextIndex

            _ -> state
    else
        state

example : Str
example =
    """
    Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11    
    """

unwrap = \res ->
    when res is
        Ok x -> x
        Err _ -> crash "bad unwrap"
