app "AoC"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }

    imports [pf.Stdout, pf.Task.{ Task }, "input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    _ <- part1 input |> List.sum |> Num.toStr |> Stdout.line |> Task.await
    _ <- part2 input |> List.sum |> Num.toStr |> Stdout.line |> Task.await

    Stdout.line "Ok"

part1 = \str ->
    line <- Str.split str "\n" |> List.map
    { before, after } = Str.splitFirst line " " |> unwrap
    groups = after |> Str.split "," |> List.keepOks Str.toNat
    partList = before |> Str.toUtf8 |> List.map toCondition
    countPossibilities partList groups (Dict.empty {}) |> .1

part2 = \str ->
    line <- Str.split str "\n" |> List.map
    { before, after } = Str.splitFirst line " " |> unwrap
    groups = after |> Str.split "," |> List.keepOks Str.toNat |> List.repeat 5 |> List.join
    partList = before |> Str.toUtf8 |> List.map toCondition |> List.repeat 5 |> List.intersperse [Unknown] |> List.join
    countPossibilities partList groups (Dict.empty {}) |> .1

countPossibilities = \parts, groups, lookup ->
    when Dict.get lookup (parts, groups) is
        Ok solution -> (lookup, solution)
        Err _ ->
            when List.first groups is
                Err _ ->
                    if List.any parts \p -> p == Damaged then
                        (Dict.insert lookup (parts, groups) 0, 0)
                    else
                        (Dict.insert lookup (parts, groups) 1, 1)

                Ok group ->
                    firstPossibleBroken = List.findFirstIndex parts \p -> p == Unknown || p == Damaged

                    when firstPossibleBroken is
                        Err _ ->
                            (Dict.insert lookup (parts, groups) 0, 0)

                        Ok n ->
                            subList = List.sublist parts { start: n, len: group }
                            if List.len subList != group then
                                (Dict.insert lookup (parts, groups) 0, 0)
                            else
                                isAfterOk =
                                    when List.get parts (n + group) is
                                        Ok Damaged -> Bool.false
                                        _ -> Bool.true
                                isPossible = List.all subList \p -> p == Unknown || p == Damaged

                                dbg
                                    {
                                        groups,
                                        group: group,
                                        partsLen: List.len parts,
                                        subList,
                                        isPossible,
                                        isAfterOk,
                                        startPos: n,
                                    }

                                if isPossible && isAfterOk then
                                    next =
                                        if
                                            List.get parts n == Ok Damaged
                                        then
                                            (Dict.insert lookup (parts, groups) 0, 0)
                                        else
                                            countPossibilities (List.dropFirst parts (n + 1)) groups lookup
                                    continue = countPossibilities (List.dropFirst parts (n + group + 1)) (List.dropFirst groups 1) next.0

                                    if continue.1 == 0 then
                                        (Dict.insert continue.0 (parts, groups) next.1, next.1)
                                    else
                                        (Dict.insert continue.0 (parts, groups) (next.1 + continue.1), next.1 + continue.1)
                                else if List.get parts n == Ok Damaged then
                                    (Dict.insert lookup (parts, groups) 0, 0)
                                else
                                    solution = countPossibilities (List.dropFirst parts (n + 1)) groups lookup
                                    (Dict.insert solution.0 (parts, groups) solution.1, solution.1)

toCondition = \c ->
    when c is
        '?' -> Unknown
        '#' -> Damaged
        '.' -> Operational
        _ -> crash "Unknown part condition"

unwrap = \res ->
    when res is
        Ok x -> x
        Err _ ->
            crash "bad unwrap"
