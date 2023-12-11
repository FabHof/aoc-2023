app "AoC"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }

    imports [pf.Stdout, pf.Task.{ Task }, "input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    Stdout.line (solve input)

solve = \str ->
    width = Str.splitFirst str "\n" |> unwrap |> .before |> Str.countUtf8Bytes |> Num.add 1
    startPos =
        Str.toUtf8 str
        |> List.findFirstIndex \c -> c == 'S'
        |> unwrap
    # Down works for both the example and my input.
    field = str |> Str.toUtf8 |> List.map \c -> { region: Unknown, char: c }
    fieldWithPipe = walkPipe field startPos Down width 0

    dbg
        P1 ((List.countIf fieldWithPipe (\cell -> cell.region == Pipe)) // 2)

    getPipesToRight = \index ->
        List.walkFromUntil
            fieldWithPipe
            (index + 1)
            { count: 0, lastC: ' ' }
            \state, cell ->
                if cell.char == '\n' then
                    Break state
                else if cell.region == Pipe then
                    if cell.char == '|' then
                        Continue { state & count: state.count + 1 }
                    else if cell.char == 'F' || cell.char == 'L' then
                        Continue { state & lastC: cell.char }
                    else if cell.char == '7' || cell.char == 'J' then
                        shouldAdd =
                            (cell.char == '7' && state.lastC == 'L')
                            || (cell.char == 'J' && state.lastC == 'F')
                        Continue { state & lastC: ' ', count: if shouldAdd then state.count + 1 else state.count }
                    else
                        Continue state
                else
                    Continue state

    countIfInside = \count, cell, index ->
        if cell.char == '\n' || cell.region == Pipe then
            count
        else
            pipesToRight = getPipesToRight index
            if pipesToRight.count % 2 == 0 then
                count
            else
                count + 1

    dbg
        P2 (List.walkWithIndex fieldWithPipe 0 countIfInside)

    "Done"

walkPipe = \field, position, direction, fieldWidth, stepcount ->
    list = List.update field position \p -> { p & region: Pipe }
    when direction is
        Down ->
            nextPosition = position + fieldWidth
            when List.get list nextPosition |> unwrap |> .char is
                '|' ->
                    walkPipe list nextPosition Down fieldWidth (stepcount + 1)

                'L' ->
                    walkPipe list nextPosition Right fieldWidth (stepcount + 1)

                'J' ->
                    walkPipe list nextPosition Left fieldWidth (stepcount + 1)

                'S' -> list
                _ -> crash "Invalid path"

        Left ->
            nextPosition = position - 1
            when List.get list nextPosition |> unwrap |> .char is
                '-' ->
                    walkPipe list nextPosition Left fieldWidth (stepcount + 1)

                'F' ->
                    walkPipe list nextPosition Down fieldWidth (stepcount + 1)

                'L' ->
                    walkPipe list nextPosition Up fieldWidth (stepcount + 1)

                'S' -> list
                _ -> crash "Invalid path"

        Right ->
            nextPosition = position + 1
            when List.get list nextPosition |> unwrap |> .char is
                '-' ->
                    walkPipe list nextPosition Right fieldWidth (stepcount + 1)

                '7' ->
                    walkPipe list nextPosition Down fieldWidth (stepcount + 1)

                'J' ->
                    walkPipe list nextPosition Up fieldWidth (stepcount + 1)

                'S' -> list
                _ -> crash "Invalid path"

        Up ->
            nextPosition = position - fieldWidth
            when List.get list nextPosition |> unwrap |> .char is
                '|' ->
                    walkPipe list nextPosition Up fieldWidth (stepcount + 1)

                'F' ->
                    walkPipe list nextPosition Right fieldWidth (stepcount + 1)

                '7' ->
                    walkPipe list nextPosition Left fieldWidth (stepcount + 1)

                'S' -> list
                _ -> crash "Invalid path"

example : Str
example =
    """
    OF----7F7F7F7F-7OOOO
    O|F--7||||||||FJOOOO
    O||OFJ||||||||L7OOOO
    FJL7L7LJLJ||LJIL-7OO
    L--JOL7IIILJS7F-7L7O
    OOOOF-JIIF7FJ|L7L7L7
    OOOOL7IF7||L7|IL7L7|
    OOOOO|FJLJ|FJ|F7|OLJ
    OOOOFJL-7O||O||||OOO
    OOOOL---JOLJOLJLJOOO
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
