app "AoC"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }

    imports [pf.Stdout, pf.Task.{ Task }, "input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    dbg part1 input
    dbg part2 input

    Stdout.line "Done"

part1 = \str ->
    lines =
        line <- str |> Str.split "\n" |> List.map
        Str.toUtf8 line |> List.splitLast ' ' |> unwrap |> .before

    startCorners = (List.reserve [{ x: 0, y: 0 }] (List.len lines), 0)

    corners = List.walk lines startCorners toCorners

    area = shoelace corners.0
    area + 1 + (corners.1 // 2)

part2 = \str ->
    lines =
        line <- str |> Str.split "\n" |> List.map
        Str.toUtf8 line |> List.splitLast '#' |> unwrap |> .after

    startCorners = (List.reserve [{ x: 0, y: 0 }] (List.len lines), 0)

    corners = List.walk lines startCorners toCorners2

    area = shoelace corners.0
    area + 1 + (corners.1 // 2)

shoelace = \corners ->
    List.walk corners (0, None) \(area, before), elem ->
        when before is
            None -> (area, Some elem)
            Some beforeElem ->
                (area + beforeElem.x * elem.y - beforeElem.y * elem.x, Some elem)
    |> .0
    |> Num.divTrunc 2

toCorners = \(corners, perimCount), currentPath ->
    lastCorner = List.last corners |> unwrap
    when currentPath is
        ['R', ' ', .. as count] ->
            steps = Str.fromUtf8 count |> unwrap |> Str.toI64 |> unwrap
            (List.append corners { lastCorner & x: lastCorner.x + steps }, perimCount + steps)

        ['L', ' ', .. as count] ->
            steps = Str.fromUtf8 count |> unwrap |> Str.toI64 |> unwrap
            (List.append corners { lastCorner & x: lastCorner.x - steps }, perimCount + steps)

        ['U', ' ', .. as count] ->
            steps = Str.fromUtf8 count |> unwrap |> Str.toI64 |> unwrap
            (List.append corners { lastCorner & y: lastCorner.y - steps }, perimCount + steps)

        ['D', ' ', .. as count] ->
            steps = Str.fromUtf8 count |> unwrap |> Str.toI64 |> unwrap
            (List.append corners { lastCorner & y: lastCorner.y + steps }, perimCount + steps)

        _ -> crash "Invalid Input"

toCorners2 = \(corners, perimCount), currentPath ->
    lastCorner = List.last corners |> unwrap
    steps = List.takeFirst currentPath 5 |> fromHex
    when List.get currentPath 5 is
        Ok '0' ->
            (List.append corners { lastCorner & x: lastCorner.x + steps }, perimCount + steps)

        Ok '2'  ->
            (List.append corners { lastCorner & x: lastCorner.x - steps }, perimCount + steps)

        Ok '3'  ->
            (List.append corners { lastCorner & y: lastCorner.y - steps }, perimCount + steps)

        Ok '1'  ->
            (List.append corners { lastCorner & y: lastCorner.y + steps }, perimCount + steps)

        _ -> crash "Invalid Input"

fromHex = \lst ->
    List.walk lst 0 (\val, byte -> val * 16 + digitToInt byte)
    
digitToInt = \d ->
    when d is 
        '0' -> 0
        '1' -> 1
        '2' -> 2
        '3' -> 3
        '4' -> 4
        '5' -> 5
        '6' -> 6
        '7' -> 7
        '8' -> 8
        '9' -> 9
        'a' -> 10
        'b' -> 11
        'c' -> 12
        'd' -> 13
        'e' -> 14
        'f' -> 15
        _   -> crash "Not hex"

example =
    """
    R 6 (#70c710)
    D 5 (#0dc571)
    L 2 (#5713f0)
    D 2 (#d2c081)
    R 2 (#59c680)
    D 2 (#411b91)
    L 5 (#8ceee2)
    U 2 (#caa173)
    L 1 (#1b58a2)
    U 2 (#caa171)
    R 2 (#7807d2)
    U 3 (#a77fa3)
    L 2 (#015232)
    U 2 (#7a21e3)
    """

unwrap = \res ->
    when res is
        Ok x -> x
        Err _ ->
            crash "bad unwrap"

debug = \x ->
    dbg x

    x
