app "AoC"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }

    imports [pf.Stdout, pf.Task.{ Task }, "input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    dbg
        part1 input

    dbg
        part2 example

    Stdout.line "Done"

part1 = \str ->
    width = Str.splitFirst str "\n" |> unwrap |> .before |> Str.countUtf8Bytes |> Num.add 1
    startPos =
        Str.toUtf8 str
        |> List.findFirstIndex \c -> c == 'S'
        |> unwrap
    # Down works for both the example and my input.
    steps = walkPipe (Str.toUtf8 str) startPos Down width 0
    steps // 2

walkPipe = \list, position, direction, fieldWidth, stepcount ->
    when direction is
        Down ->
            nextPosition = position + fieldWidth
            when List.get list nextPosition is
                Ok '|' ->
                    walkPipe list nextPosition Down fieldWidth (stepcount + 1)

                Ok 'L' ->
                    walkPipe list nextPosition Right fieldWidth (stepcount + 1)

                Ok 'J' ->
                    walkPipe list nextPosition Left fieldWidth (stepcount + 1)

                Ok 'S' -> (stepcount + 1)
                _ -> crash "Invalid path"

        Left ->
            nextPosition = position - 1
            when List.get list nextPosition is
                Ok '-' ->
                    walkPipe list nextPosition Left fieldWidth (stepcount + 1)

                Ok 'F' ->
                    walkPipe list nextPosition Down fieldWidth (stepcount + 1)

                Ok 'L' ->
                    walkPipe list nextPosition Up fieldWidth (stepcount + 1)

                Ok 'S' -> (stepcount + 1)
                _ -> crash "Invalid path"

        Right ->
            nextPosition = position + 1
            when List.get list nextPosition is
                Ok '-' ->
                    walkPipe list nextPosition Right fieldWidth (stepcount + 1)

                Ok '7' ->
                    walkPipe list nextPosition Down fieldWidth (stepcount + 1)

                Ok 'J' ->
                    walkPipe list nextPosition Up fieldWidth (stepcount + 1)

                Ok 'S' -> (stepcount + 1)
                _ -> crash "Invalid path"

        Up ->
            nextPosition = position - fieldWidth
            when List.get list nextPosition is
                Ok '|' ->
                    walkPipe list nextPosition Up fieldWidth (stepcount + 1)

                Ok 'F' ->
                    walkPipe list nextPosition Right fieldWidth (stepcount + 1)

                Ok '7' ->
                    walkPipe list nextPosition Left fieldWidth (stepcount + 1)

                Ok 'S' -> (stepcount + 1)
                _ -> crash "Invalid path"

part2 = \str ->
    "TODO"

example : Str
example =
    """
    -L|F7
    7S-7|
    L|7||
    -L-J|
    L|-JF
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
