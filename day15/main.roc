app "AoC"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }

    imports [pf.Stdout, pf.Task.{ Task }, "input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    dbg part1 input

    dbg part2 input

    Stdout.line "Ok"

part1 = \str ->
    Str.split str "," |> List.map Str.toUtf8 |> List.map toHash |> List.sum

part2 = \str ->
    Str.split str ","
    |> List.walk (List.repeat [] 256) fillHashMap
    |> List.map
        (\innerList ->
            List.walkWithIndex innerList 0 (\sum, (_, v), index -> sum + (index + 1) * (Num.toNat v)))
    |> List.walkWithIndex 0 (\sum, box, index -> sum + (index + 1) * box)

fillHashMap = \list, item ->
    key = Str.toUtf8 item |> List.keepIf (\c -> c >= 'a' && c <= 'z')
    hash = key |> toHash
    if Str.toUtf8 item |> List.get (List.len key) |> unwrap == '-' then
        List.update list hash (\innerList -> List.dropIf innerList (\(k, _) -> k == key))
    else
        val = Str.toUtf8 item |> List.get (List.len key |> Num.add 1) |> unwrap |> Num.sub '0'
        innerList = List.get list hash |> unwrap
        newInnerList =
            when List.findFirstIndex innerList (\(k, _) -> k == key) is
                Ok i -> List.set innerList i (key, val)
                Err _ -> List.append innerList (key, val)

        List.set list hash newInnerList

toHash = \str ->
    List.walk
        str
        0
        (\val, char ->
            foo = ((val + (char |> Num.toNat)) * 17)
            foo % 256
        )

example =
    "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

unwrap = \res ->
    when res is
        Ok x -> x
        Err _ ->
            crash "bad unwrap"

debug = \x ->
    dbg x

    x
