app "AoC"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }

    imports [pf.Stdout, pf.Task.{ Task }, "input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    _ <- part1 input |> Num.toStr |> Stdout.line |> Task.await
    part2 input |> Num.toStr |> Stdout.line

part1 = \str ->
    Str.split str "\n\n"
    |> List.map (\p -> 
        when toReflectionNumber p is 
            One n -> n
            _ -> crash "invalid input")
    |> List.sum

part2 = \str ->
    Str.split str "\n\n"
    |> List.map toSmudgedReflectionNumber
    |> List.sum

toSmudgedReflectionNumber = \str ->
    list = Str.toUtf8 str
    when toReflectionNumber str is
        One num -> findSmudgedReflectionNumber list 0 num
        _ -> crash "invalid input"

findSmudgedReflectionNumber = \str, pos, oldNum ->
    smudge = List.get str pos
    if smudge == Ok '\n' then
        findSmudgedReflectionNumber str (pos + 1) oldNum
    else
        newStr =
            when smudge is
                Ok '.' -> List.set str pos '#' |> Str.fromUtf8 |> unwrap
                Ok '#' -> List.set str pos '.' |> Str.fromUtf8 |> unwrap
                _ -> crash "invalid input"
        when toReflectionNumber newStr is
            None -> findSmudgedReflectionNumber str (pos + 1) oldNum
            One n ->
                if n == oldNum then
                    findSmudgedReflectionNumber str (pos + 1) oldNum
                else
                    n

            Two a b ->
                if a == oldNum then
                    b
                else
                    a

toReflectionNumber = \str ->
    when findVerticalReflectionLines (Str.split str "\n") 1 None is
        None -> findHorizontalReflectionLines (Str.split str "\n") 1 None
        Two a b -> Two a b
        One a -> findHorizontalReflectionLines (Str.split str "\n") 1 (One a)

findHorizontalReflectionLines = \list, num, found ->
    if num == List.len list then
        found
    else
        split = List.split list num
        toDrop = Num.subSaturated (num + num) (List.len list)
        valid =
            List.map2
                (List.dropFirst split.before toDrop |> List.reverse)
                split.others
                (\a, b -> a == b)
            |> List.all (\a -> a)
        if valid then
            when found is
                None -> findHorizontalReflectionLines list (num + 1) (One (num * 100))
                One a -> Two a (num * 100)
                Two _ _ -> crash "found 3"
        else
            findHorizontalReflectionLines list (num + 1) found

findVerticalReflectionLines = \list, num, found ->
    if num == (List.get list 0 |> unwrap |> Str.countUtf8Bytes) then
        found
    else
        split =
            List.map list Str.toUtf8
            |> List.map (\l -> List.split l num)

        toDrop = Num.subSaturated (num + num) (List.get list 0 |> unwrap |> Str.countUtf8Bytes)
        valid =
            List.map2
                (List.map split .before |> List.map (\x -> List.dropFirst x toDrop |> List.reverse))
                (List.map split .others)
                (\a, b -> (List.startsWith b a) || a == b)
            |> List.all (\a -> a)
        if valid then
            when found is
                None -> findVerticalReflectionLines list (num + 1) (One num)
                One a -> Two a num
                Two _ _ -> crash "found 3"
        else
            findVerticalReflectionLines list (num + 1) found

example =
    """
    #.##..##.
    ..#.##.#.
    ##......#
    ##......#
    ..#.##.#.
    ..##..##.
    #.#.##.#.

    #...##..#
    #....#..#
    ..##..###
    #####.##.
    #####.##.
    ..##..###
    #....#..#
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
