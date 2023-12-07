app "AoC"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }

    imports [pf.Stdout, pf.Task.{ Task }, "input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    dbg
        P1 (part1 input)

    dbg
        P2 (part2 input)

    Stdout.line "done"

part1 = \str ->
    str
    |> Str.split "\n"
    |> List.sortWith compareHands
    |> List.map (\hand -> Str.splitFirst hand " " |> unwrap |> .after |> Str.toNat |> unwrap)
    |> List.walkWithIndex 0 (\total, card, index -> total + card * (index + 1))

part2 = \str ->
    str
    |> Str.split "\n"
    |> List.sortWith compareHands2
    |> List.map (\hand -> Str.splitFirst hand " " |> unwrap |> .after |> Str.toNat |> unwrap)
    |> List.walkWithIndex 0 (\total, card, index -> total + card * (index + 1))

compareHands = \hand1, hand2 ->
    getHandCards = \str -> Str.splitFirst str " " |> unwrap |> .before
    getCardCounts = \str ->
        Str.walkUtf8
            str
            (Dict.empty {})
            (\dict, char ->
                Dict.update
                    dict
                    char
                    (\oldval ->
                        when oldval is
                            Missing -> Present 1
                            Present x -> Present (x + 1)))
    isFullHouse = \dict -> Dict.len dict == 2
    isTwoPairs = \dict -> (Dict.values dict |> List.countIf (\x -> x == 2)) == 2
    hand1Cards = getHandCards hand1
    hand2Cards = getHandCards hand2
    hand1CardCounts = getCardCounts hand1Cards
    hand2CardCounts = getCardCounts hand2Cards
    if hand1CardCounts |> Dict.values |> List.max |> unwrap > hand2CardCounts |> Dict.values |> List.max |> unwrap then
        GT
    else if hand1CardCounts |> Dict.values |> List.max |> unwrap < hand2CardCounts |> Dict.values |> List.max |> unwrap then
        LT
    else if isFullHouse hand1CardCounts && !(isFullHouse hand2CardCounts) then
        GT
    else if isFullHouse hand2CardCounts && !(isFullHouse hand1CardCounts) then
        LT
    else if isTwoPairs hand1CardCounts && !(isTwoPairs hand2CardCounts) then
        GT
    else if isTwoPairs hand2CardCounts && !(isTwoPairs hand1CardCounts) then
        LT
    else
        compareCardOrder hand1Cards hand2Cards

compareHands2 = \hand1, hand2 ->
    getHandCards = \str -> Str.splitFirst str " " |> unwrap |> .before
    getCardCounts = \str ->
        Str.walkUtf8
            str
            (Dict.empty {})
            (\dict, char ->
                Dict.update
                    dict
                    char
                    (\oldval ->
                        when oldval is
                            Missing -> Present 1
                            Present x -> Present (x + 1)))
    isFullHouse = \dict -> Dict.len dict == 2 || (Dict.len dict == 3 && Dict.contains dict 'J')
    isTwoPairs = \dict -> (Dict.values dict |> List.countIf (\x -> x == 2)) == 2
    getMaxCount = \dict -> (Dict.remove dict 'J' |> Dict.values |> List.max |> Result.withDefault 0) + (Dict.get dict 'J' |> Result.withDefault 0)
    hand1Cards = getHandCards hand1
    hand2Cards = getHandCards hand2
    hand1CardCounts = getCardCounts hand1Cards
    hand2CardCounts = getCardCounts hand2Cards

    if getMaxCount hand1CardCounts > getMaxCount hand2CardCounts then
        GT
    else if getMaxCount hand1CardCounts < getMaxCount hand2CardCounts then
        LT
    else if isFullHouse hand1CardCounts && !(isFullHouse hand2CardCounts) then
        GT
    else if isFullHouse hand2CardCounts && !(isFullHouse hand1CardCounts) then
        LT
    else if isFullHouse hand2CardCounts && isFullHouse hand1CardCounts then
        compareCardOrder2 hand1Cards hand2Cards
    else if isTwoPairs hand1CardCounts && !(isTwoPairs hand2CardCounts) then
        GT
    else if isTwoPairs hand2CardCounts && !(isTwoPairs hand1CardCounts) then
        LT
    else
        compareCardOrder2 hand1Cards hand2Cards

compareCardOrder = \hand1, hand2 ->
    List.map2 (hand1 |> Str.toUtf8) (hand2 |> Str.toUtf8) compareCardValue
    |> List.findFirst (\v -> v != EQ)
    |> Result.withDefault EQ

compareCardValue = \c1, c2 ->
    mapToValue = \c ->
        when c is
            'A' -> 14
            'K' -> 13
            'Q' -> 12
            'J' -> 11
            'T' -> 10
            _ -> c - '0'
    Num.compare (mapToValue c1) (mapToValue c2)

compareCardOrder2 = \hand1, hand2 ->
    List.map2 (hand1 |> Str.toUtf8) (hand2 |> Str.toUtf8) compareCardValue2
    |> List.findFirst (\v -> v != EQ)
    |> Result.withDefault EQ

compareCardValue2 = \c1, c2 ->
    mapToValue = \c ->
        when c is
            'A' -> 14
            'K' -> 13
            'Q' -> 12
            'J' -> 0
            'T' -> 10
            _ -> c - '0'
    Num.compare (mapToValue c1) (mapToValue c2)

example : Str
example =
    """
    32T3K 765
    T55J5 684
    KK677 28
    KTJJT 220
    QQQJA 483
    """

testInput =
    """
    AJATT 614
    A2777 634
    1234J 0
    12344 0
    12345 0
    9232J 0
    55512 0
    JKKQQ 0
    QQQKK 0
    KJKQQ 1
    JJJJJ 2
    JJJJK 3
    JJJKK 4
    KKKKK 5
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
