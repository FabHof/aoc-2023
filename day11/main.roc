app "AoC"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }

    imports [pf.Stdout, pf.Task.{ Task }, "input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    Stdout.line "Part1: \(solve input P1 |> Num.toStr), Part2: \(solve input P2 |> Num.toStr)"

solve = \str, part ->
    rowLength =
        Str.toUtf8 str
        |> List.findFirstIndex \c -> c == '\n'
        |> unwrap
        |> Num.add 1
    galaxies = Str.walkUtf8WithIndex
        str
        []
        (\pos, char, index ->
            if char == '#' then
                List.append pos { x: index % rowLength, y: index // rowLength }
            else
                pos
        )

    expandRows = \galax, currentRow ->
        if currentRow > List.map galax .y |> List.max |> unwrap then
            galax
        else if List.findFirst galax (\g -> g.y == currentRow) == Err NotFound then
            newGlax = List.map galax (\g -> if g.y > currentRow then { g & y: g.y + if part == P1 then 1 else 999999 } else g)
            expandRows newGlax (currentRow + if part == P1 then 2 else 1000000)
        else
            expandRows galax (currentRow + 1)

    expandColumns = \galax, currentColumn ->
        if currentColumn > List.map galax .x |> List.max |> unwrap then
            galax
        else if List.findFirst galax (\g -> g.x == currentColumn) == Err NotFound then
            newGlax = List.map galax (\g -> if g.x > currentColumn then { g & x: g.x + if part == P1 then 1 else 999999 } else g)
            expandColumns newGlax (currentColumn + if part == P1 then 2 else 1000000)
        else
            expandColumns galax (currentColumn + 1)

    expandedGalaxies = expandRows galaxies 0 |> expandColumns 0
    sumDistances expandedGalaxies

sumDistances = \galaxies ->
    distance = \g1, g2 ->
        (Num.absDiff g2.x g1.x) + (Num.absDiff g2.y g1.y)
    when galaxies is
        [g1, .. as rest] -> List.map rest (\g -> distance g1 g) |> List.sum |> Num.add (sumDistances rest)
        [] -> 0

example : Str
example =
    """
    ...#......
    .......#..
    #.........
    ..........
    ......#...
    .#........
    .........#
    ..........
    .......#..
    #...#.....    
    """

unwrap = \res ->
    when res is
        Ok x -> x
        Err _ ->
            dbg
                res

            crash "bad unwrap"

debug = \x ->
    dbg
        x

    x
