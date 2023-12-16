app "AoC"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }

    imports [pf.Stdout, pf.Task.{ Task }, "input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    # dbg
    #     part1 input

    foo = part2 example
    dbg foo

    Stdout.line "Ok"

part1 = \str ->
    lines = Str.split str "\n" |> List.map Str.toUtf8
    List.range
        { start: At 0, end: Before (List.len (List.get lines 0 |> unwrap)) }
    |> List.map \i -> count lines i (List.len lines) (List.len lines)
    |> List.sum

count = \lines, i, w, lc ->
    when lines is
        [line, .. as rest] ->
            when List.get line i is
                Ok 'O' ->
                    w + (count rest i (w - 1) (lc - 1))

                Ok '#' ->
                    count rest i (lc - 1) (lc - 1)

                Ok '.' ->
                    count rest i w (lc - 1)

                _ -> crash "invalid Input"

        _ -> 0

part2 = \str ->
    lines = Str.split str "\n" |> List.map Str.toUtf8
    maxY = List.len lines
    maxX = List.get lines 0 |> unwrap |> List.len
    startState = { boulders: [], obstacles: [] }
    field = List.walkWithIndex
        lines
        startState
        (\state, line, yIndex ->
            List.walkWithIndex
                line
                state
                (\s, c, xIndex ->
                    when c is
                        'O' -> { s & boulders: List.append s.boulders (xIndex |> Num.toI64, yIndex |> Num.toI64) }
                        '#' -> { s & obstacles: List.append s.obstacles (xIndex |> Num.toI64, yIndex |> Num.toI64) }
                        _ -> s
                )
        )

    calcFieldUntilConstant field maxX maxY 0


calcFieldUntilConstant = \f, maxX, maxY, i->
    dbg i
    newField =
        f
        |> north maxX
        |> west maxY
        |> south maxX maxY
        |> east maxX maxY
    if f.boulders == newField.boulders then
        dbg FOUND
        newField.boulders
    else
        dbg NEXT
        calcFieldUntilConstant newField maxX maxY (i +  1)
    
north = \field, maxX ->
    newBoulders =
        List.range { start: At 0, end: Length maxX }
        |> List.joinMap
            (\i ->
                boulders = List.keepIf field.boulders (\b -> b.0 == i) |> List.map .1
                obstacles = List.keepIf field.obstacles (\o -> o.0 == i) |> List.map .1 |> List.sortAsc
                startState = {
                    currentObstacle: -1,
                    boulderPos: [],
                }
                newBouldersPart = List.walk obstacles startState \state, o ->
                    boulderCount = List.keepIf boulders (\b -> b < o && b > state.currentObstacle) |> List.len
                    newBoulderPos = List.range { start: After state.currentObstacle, end: Length boulderCount }
                    { currentObstacle: o, boulderPos: List.concat state.boulderPos newBoulderPos }
                biggerBoulderCount = List.keepIf boulders (\b -> b > newBouldersPart.currentObstacle) |> List.len
                biggerNewBoulderPos = List.range { start: After newBouldersPart.currentObstacle, end: Length biggerBoulderCount }
                List.concat newBouldersPart.boulderPos biggerNewBoulderPos
                |> List.map (\y -> (i, y))
            )
    { field & boulders: newBoulders }

west = \field, maxY ->
    newBoulders =
        List.range { start: At 0, end: Length maxY }
        |> List.joinMap
            (\i ->
                boulders = List.keepIf field.boulders (\b -> b.1 == i) |> List.map .0
                obstacles = List.keepIf field.obstacles (\o -> o.1 == i) |> List.map .0 |> List.sortAsc
                startState = {
                    currentObstacle: -1,
                    boulderPos: [],
                }
                newBouldersPart = List.walk obstacles startState \state, o ->
                    boulderCount = List.keepIf boulders (\b -> b < o && b > state.currentObstacle) |> List.len
                    newBoulderPos = List.range { start: After state.currentObstacle, end: Length boulderCount }
                    { currentObstacle: o, boulderPos: List.concat state.boulderPos newBoulderPos }
                biggerBoulderCount = List.keepIf boulders (\b -> b > newBouldersPart.currentObstacle) |> List.len
                biggerNewBoulderPos = List.range { start: After newBouldersPart.currentObstacle, end: Length biggerBoulderCount }
                List.concat newBouldersPart.boulderPos biggerNewBoulderPos
                |> List.map (\x -> (x, i))
            )
    { field & boulders: newBoulders }

south = \field, maxX, maxY ->
    newBoulders =
        List.range { start: At 0, end: Length maxX }
        |> List.joinMap
            (\i ->
                boulders = List.keepIf field.boulders (\b -> b.0 == i) |> List.map .1
                obstacles = List.keepIf field.obstacles (\o -> o.0 == i) |> List.map .1 |> List.sortDesc
                startState = {
                    currentObstacle: maxY |> Num.toI64,
                    boulderPos: [],
                }
                newBouldersPart = List.walk obstacles startState \state, o ->
                    boulderCount = List.keepIf boulders (\b -> b > o && b < state.currentObstacle) |> List.len |> Num.toI64
                    newBoulderPos = List.range { start: At (state.currentObstacle - boulderCount), end: Length (boulderCount |> Num.toNat) }
                    { currentObstacle: o, boulderPos: List.concat state.boulderPos newBoulderPos }
                smalerBoulderCount = List.keepIf boulders (\b -> b < newBouldersPart.currentObstacle) |> List.len |> Num.toI64
                biggerNewBoulderPos = List.range { start: At (newBouldersPart.currentObstacle - smalerBoulderCount), end: Length (smalerBoulderCount |> Num.toNat) }
                List.concat newBouldersPart.boulderPos biggerNewBoulderPos
                |> List.map (\y -> (i, y))
            )
    { field & boulders: newBoulders }

east = \field, maxX, maxY ->
    newBoulders =
        List.range { start: At 0, end: Length maxY }
        |> List.joinMap
            (\i ->
                boulders = List.keepIf field.boulders (\b -> b.1 == i) |> List.map .0
                obstacles = List.keepIf field.obstacles (\o -> o.1 == i) |> List.map .0 |> List.sortDesc
                startState = {
                    currentObstacle: maxX |> Num.toI64,
                    boulderPos: [],
                }
                newBouldersPart = List.walk obstacles startState \state, o ->
                    boulderCount = List.keepIf boulders (\b -> b > o && b < state.currentObstacle) |> List.len |> Num.toI64
                    newBoulderPos = List.range { start: At (state.currentObstacle - boulderCount), end: Length (boulderCount |> Num.toNat) }
                    { currentObstacle: o, boulderPos: List.concat state.boulderPos newBoulderPos }
                smalerBoulderCount = List.keepIf boulders (\b -> b < newBouldersPart.currentObstacle) |> List.len |> Num.toI64
                biggerNewBoulderPos = List.range { start: At (newBouldersPart.currentObstacle - smalerBoulderCount), end: Length (smalerBoulderCount |> Num.toNat) }
                List.concat newBouldersPart.boulderPos biggerNewBoulderPos
                |> List.map (\x -> (x, i))
            )
    { field & boulders: newBoulders }

example =
    """
    O....#....
    O.OO#....#
    .....##...
    OO.#O....O
    .O.....O#.
    O.#..O.#.#
    ..O..#O..O
    .......O..
    #....###..
    #OO..#....
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
