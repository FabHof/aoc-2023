app "AoC"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }

    imports [pf.Stdout, pf.Task.{ Task }, "day-2-input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    p1 = part1 input |> Num.toStr

    p2 = part2 input |> Num.toStr

    Stdout.line "Part 1: \(p1) \nPart 2: \(p2)"

part1 : Str -> U32
part1 = \data ->
    data
    |> Str.split "\n"
    |> List.map \line ->
        { before, after } = Str.splitFirst line ":" |> unwrap
        valid = lineValid after
        if valid then
            before
            |> Str.splitFirst " "
            |> unwrap
            |> .after
            |> Str.toU32
            |> unwrap
        else
            0
    |> List.sum

lineValid = \after ->
    set <- Str.split after ";" |> List.all
    cubes <- set |> Str.trim |> Str.split "," |> List.all
    { before: countStr, after: color } =
        cubes
        |> Str.trim
        |> Str.splitFirst " "
        |> unwrap

    count = Str.toU32 countStr |> unwrap
    when color is
        "red" -> count <= 12
        "green" -> count <= 13
        "blue" -> count <= 14
        _ -> crash "invalid color: \(color)"

part2 : Str -> U32
part2 = \data ->
    data
    |> Str.split "\n"
    |> List.map \line ->
        { after } = Str.splitFirst line ":" |> unwrap
        power after
    |> List.sum

power : Str -> U32
power = \game ->
    { r, b, g } =
        Str.split game ";"
        |> List.map \set ->
            set
            |> Str.trim
            |> Str.split ","
            |> List.map \cubes ->
                { before: countStr, after: color } =
                    cubes
                    |> Str.trim
                    |> Str.splitFirst " "
                    |> unwrap
                count = Str.toU32 countStr |> unwrap
                when color is
                    "red" -> { r: count, g: 0, b: 0 }
                    "green" -> { r: 0, g: count, b: 0 }
                    "blue" -> { r: 0, g: 0, b: count }
                    _ -> crash "invalid color: \(color)"
        |> List.join
        |> List.walk { r: 0, b: 0, g: 0 } \old, new -> { r: Num.max old.r new.r, g: Num.max old.g new.g, b: Num.max old.b new.b }
    r * b * g

example : Str
example =
    """
    Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
    Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
    Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
    Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
    """

unwrap = \res ->
    when res is
        Ok x -> x
        Err _ -> crash "bad unwrap"
