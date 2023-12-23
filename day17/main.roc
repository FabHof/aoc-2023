app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.1.1/gvYudeXPZL33PDh5jRxXOPbeaQV5kLZAsgecc68HBOA.tar.br",
    }

    imports [
        pf.Stdout,
        pf.Task.{ Task },
        array2d.Array2D.{ Array2D },
        "input.txt" as input : Str,
    ]
    provides [main] to pf

main : Task {} *
main =
    solved = part1 input
    Stdout.line (Num.toStr solved)

part1 = \str ->
    grid =
        Str.split str "\n"
        |> List.map (\s -> Str.toUtf8 s |> List.map (\c -> c - '0'))
        |> Array2D.fromExactLists
        |> unwrap

    initialState = { coord: { x: 0, y: 0 }, heat: 0, dir: { x: 0, y: 0 }, moves: 0 }
    queue = [initialState] |> List.reserve 5000
    shape = Array2D.shape grid
    target = { x: shape.dimX - 1, y: shape.dimY - 1 }

    solved = solve grid queue (Set.withCapacity (Str.countUtf8Bytes str)) target

    solved

solve = \grid, q, v, target ->
    bfs = \queue, visited ->
        when queue is
            [state, .. as rest] ->
                if state.coord == target then
                    state.heat
                else if Set.contains visited (state.coord, state.dir, state.moves) then
                    bfs rest visited
                else
                    newStates = generateNewStates state grid
                    newQueue = rest |> List.concat newStates |> List.sortWith (\a, b -> Num.compare a.heat b.heat)
                    bfs newQueue (Set.insert visited (state.coord, state.dir, state.moves))

            [] -> 0
    bfs q v

generateNewStates = \state, grid ->
    opposite = getOpposite state.dir
    directions =
        [
            { x: 1, y: 0 },
            { x: 0, y: -1 },
            { x: -1, y: 0 },
            { x: 0, y: 1 },
        ]
        |> List.dropIf (\c -> c == opposite)
    List.keepOks directions (\d -> nextState state grid d)

getOpposite = \{ x, y } ->
    { x: -x, y: -y }

nextState = \{ coord, heat, dir, moves }, grid, direction ->
    iCoord = toI32 coord
    newCoord = { x: iCoord.x + direction.x, y: iCoord.y + direction.y }
    if isValid newCoord grid && (dir != direction || moves < 3) then
        natCoord = toNat newCoord
        when Array2D.get grid natCoord is
            Ok value ->
                newHeat = heat + (Num.toNat value)
                newMoves = if dir == direction then moves + 1 else 1

                Ok { coord: natCoord, heat: newHeat, dir: direction, moves: newMoves }

            _ -> Err Invalid
    else
        Err Invalid

toNat = \{ x, y } ->
    { x: Num.toNat x, y: Num.toNat y }

toI32 = \{ x, y } ->
    { x: x |> Num.toI32, y: Num.toI32 y }

isValid = \{ x, y }, grid ->
    shape = Array2D.shape grid
    x >= 0 && x < (Num.toI32 shape.dimX) && y >= 0 && x < (Num.toI32 shape.dimY)

example =
    """
    2413432311323
    3215453535623
    3255245654254
    3446585845452
    4546657867536
    1438598798454
    4457876987766
    3637877979653
    4654967986887
    4564679986453
    1224686865563
    2546548887735
    4322674655533
    """

unwrap = \res ->
    when res is
        Ok x -> x
        Err _ ->
            crash "bad unwrap"

debug = \x ->
    dbg x

    x
