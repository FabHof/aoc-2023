app "AoC"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }

    imports [pf.Stdout, pf.Task.{ Task }, "input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    _ <- part1 input |> Num.toStr |> Stdout.line |> Task.await
    part2 input |> Num.toStr |> Stdout.line

part1 = \str ->
    rowStep = Str.toUtf8 str |> List.findFirstIndex (\c -> c == '\n') |> unwrap |> Num.add 1
    countEnergizedTiles str 0 Right rowStep

part2 = \str ->
    rowStep = Str.toUtf8 str |> List.findFirstIndex (\c -> c == '\n') |> unwrap |> Num.add 1

    right = List.range {start: At 0, end: Before (Str.countUtf8Bytes str), step: rowStep}
        |> List.map (\i -> countEnergizedTiles str i Right rowStep)
        |> List.max
        |> unwrap
    
    left = List.range {start: At (rowStep - 2), end: Before (Str.countUtf8Bytes str), step: rowStep}
        |> List.map (\i -> countEnergizedTiles str i Right rowStep)
        |> List.max
        |> unwrap

    down = List.range {start: At 0, end: Before (rowStep - 1)}
        |> List.map (\i -> countEnergizedTiles str i Down rowStep)
        |> List.max
        |> unwrap

    up = List.range {start: After ((Str.countUtf8Bytes str) - rowStep), end: Before (Str.countUtf8Bytes str)}
        |> List.map (\i -> countEnergizedTiles str i Down rowStep)
        |> List.max
        |> unwrap

    [left, right, up, down] |> List.max |> unwrap

countEnergizedTiles = \str, pos, dir, rowStep ->
    getEnergizedTiles (Str.toUtf8 str) pos dir rowStep (Set.empty {})
    |> Set.map (\(_, p) -> p)
    |> Set.len

getEnergizedTiles = \str, pos, dir, rowStep, energized ->
    if Set.contains energized (dir, pos) then
        energized
    else
        nextDir =
            when List.get str pos is
                Ok '.' -> One dir
                Ok '|' ->
                    when dir is
                        Right | Left -> Split Up Down
                        Up | Down -> One dir

                Ok '-' ->
                    when dir is
                        Up | Down -> Split Left Right
                        _ -> One dir

                Ok '/' ->
                    when dir is
                        Right -> One Up
                        Left -> One Down
                        Up -> One Right
                        Down -> One Left

                Ok '\\' ->
                    when dir is
                        Right -> One Down
                        Left -> One Up
                        Up -> One Left
                        Down -> One Right

                _ -> End

        when nextDir is
            One a ->
                newEnergized = Set.insert energized (dir, pos)
                when nextPos pos a rowStep is
                    Ok nPos -> getEnergizedTiles str nPos a rowStep newEnergized
                    Err _ -> newEnergized

            Split a b ->
                newEnergized = Set.insert energized (dir, pos)
                aEnergized =
                    when nextPos pos a rowStep is
                        Ok nPos -> getEnergizedTiles str nPos a rowStep newEnergized
                        Err _ -> newEnergized
                when nextPos pos b rowStep is
                    Ok nPos -> getEnergizedTiles str nPos b rowStep aEnergized
                    Err _ -> aEnergized

            End -> energized

nextPos = \pos, dir, rowStep ->
    when dir is
        Right -> Ok (pos + 1)
        Left -> Num.subChecked pos 1
        Up -> Num.subChecked pos rowStep
        Down -> Ok (pos + rowStep)

example =
    """
    .|...\\....
    |.-.\\.....
    .....|-...
    ........|.
    ..........
    .........\\
    ..../.\\\\..
    .-.-/..|..
    .|....-|.\\
    ..//.|....
    """

unwrap = \res ->
    when res is
        Ok x -> x
        Err _ ->
            crash "bad unwrap"

debug = \x ->
    dbg x

    x
