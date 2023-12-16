app "AoC"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }

    imports [pf.Stdout, pf.Task.{ Task }, "input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    dbg part1 example

    Stdout.line "Ok"

part1 = \str ->
    rowStep = Str.toUtf8 str |> List.findFirstIndex (\c -> c == '\n') |> unwrap |> Num.add 1
    getEnergizedTiles (Str.toUtf8 str) 0 Right rowStep (Set.empty {})

getEnergizedTiles = \str, pos, dir, rowStep, energized ->
    nextDir =
        when (List.get str pos, dir) is
            (Ok '.', _) | (Ok '-', Right) | (Ok '-', Left) | (Ok '|', Up) | (Ok '|', Down) -> One dir
            (Ok '|', Right) | (Ok '|', Left) -> Split Up Down
            (Ok '-', Up) | (Ok '-', Down) -> Split Left Right
            (Ok '/', Right) -> One Up
            (Ok '/', Left) -> One Down
            (Ok '/', Up) -> One Right
            (Ok '/', Down) -> One Left
            (Ok '\\', Right) -> One Down
            (Ok '\\', Left) -> One Up
            (Ok '\\', Up) -> One Left
            (Ok '\\', Down) -> Onw Right
            _ -> None

    when nextDir is
        One a -> getEnergizedTiles str (nextPos pos a rowStep) a rowStep (Set.insert energized pos)
        Two a b ->
            newEnergized = Set.insert energized pos
            aEnergized = getEnergizedTiles str (nextPos pos a rowStep) a rowStep newEnergized
            bEnergized = getEnergizedTiles str (nextPos pos b rowStep) b rowStep newEnergized
            Set.union aEnergized bEnergized

        None -> energized

nextPos = \pos, dir, rowStep ->
    when dir is
        Right -> pos + 1
        Left -> pos - 1
        Up -> pos - rowStep
        Down -> pos + rowStep

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
