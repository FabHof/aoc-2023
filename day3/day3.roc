app "AoC"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }

    imports [pf.Stdout, pf.Task.{ Task }, "day-3-input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    dbg
        P1 (part1 input)

    dbg
        P2 (part2 input)

    Stdout.line "done"

part1 = \str ->
    s = Str.toUtf8 str
    lineLength =
        List.findFirstIndex s (\c -> c == '\n')
        |> unwrap
        |> Num.add 1

    startState = {
        row: 0,
        sum: 0,
        currentNumber: 0,
        shouldAdd: Bool.false,

    }
    s
    |> List.walkWithIndex startState \state, elem, index -> sumPartNumbers state elem s index lineLength

part2 = \str ->
    s = Str.toUtf8 str
    lineLength =
        List.findFirstIndex s (\c -> c == '\n')
        |> unwrap
        |> Num.add 1

    startState = 0
    List.walkWithIndex s startState \state, elem, index -> sumGearRatio state elem s index lineLength

sumGearRatio = \state, elem, inp, index, lineLength ->
    if elem == '*' then
        nearNumbers = getNumbersNear inp index lineLength
        if List.len nearNumbers == 2 then
            state + List.product nearNumbers
        else
            state
    else
        state

getNumbersNear = \list, index, lineLength ->
    getToRight = \pos ->
        if isDigitAt list pos then
            List.dropFirst list pos
            |> takeWhile isDigit
            |> Str.fromUtf8
            |> unwrap
            |> Str.toNat
            |> unwrap
            |> Ok
        else
            Err NoNumber
    getToLeft = \pos ->
        if isDigitAt list pos then
            List.takeFirst list (pos + 1)
            |> takeWhileBackwards isDigit
            |> Str.fromUtf8
            |> unwrap
            |> Str.toNat
            |> unwrap
            |> Ok
        else
            Err NoNumber

    right = getToRight (index + 1)
    left = getToLeft (index - 1)

    topindex = Num.subWrap index lineLength
    # extracting this as a closure creates a compiler error, that's why it is copied for bottom
    top =
        if isDigitAt list topindex then
            # only one possible
            leftPart =
                List.takeFirst list topindex
                |> takeWhileBackwards isDigit
            rightPart =
                List.dropFirst list topindex
                |> takeWhile isDigit
            List.concat leftPart rightPart
            |> Str.fromUtf8
            |> unwrap
            |> Str.toNat
            |> unwrap
            |> List.single
        else
            [getToRight (topindex + 1), getToLeft (topindex - 1)]
            |> List.keepOks \x -> x

    bottomindex = index + lineLength
    bottom =
        if isDigitAt list bottomindex then
            # only one possible
            leftPart =
                List.takeFirst list bottomindex
                |> takeWhileBackwards isDigit
            rightPart =
                List.dropFirst list bottomindex
                |> takeWhile isDigit
            List.concat leftPart rightPart
            |> Str.fromUtf8
            |> unwrap
            |> Str.toNat
            |> unwrap
            |> List.single
        else
            [getToRight (bottomindex + 1), getToLeft (bottomindex - 1)]
            |> List.keepOks \x -> x
    [right, left]
    |> List.keepOks \x -> x
    |> List.concat top
    |> List.concat bottom

takeWhile = \list, cond ->
    when List.findFirstIndex list \elem -> !(cond elem) is
        Ok firstWrongIndex -> List.takeFirst list firstWrongIndex
        Err NotFound -> list

takeWhileBackwards = \list, cond ->
    when List.findLastIndex list \elem -> !(cond elem) is
        Ok lastWrongIndex -> List.dropFirst list (lastWrongIndex + 1)
        Err NotFound -> list

isDigitAt = \list, index ->
    when List.get list index is
        Err _ -> Bool.false
        Ok d -> isDigit d

sumPartNumbers = \state, elem, inp, index, lineLength ->
    if elem == '\n' then
        if state.shouldAdd then
            newState = addToSum state
            { newState & row: state.row + 1 }
        else
            { state & row: state.row + 1, currentNumber: 0 }
    else if isDigit elem then
        { state & currentNumber: state.currentNumber * 10 + (Num.toNat elem) - '0', shouldAdd: state.shouldAdd || hasSymbolNear inp index lineLength }
    else if state.shouldAdd then
        addToSum state
    else
        { state & currentNumber: 0 }

hasSymbolNear = \list, index, lineLength ->
    isSymbolAt list (index + 1)
    || isSymbolAt list (Num.subWrap index 1)
    || isSymbolAt list (index + lineLength)
    || isSymbolAt list (index + lineLength - 1)
    || isSymbolAt list (index + lineLength + 1)
    || isSymbolAt list (Num.subWrap index lineLength)
    || isSymbolAt list (Num.subWrap index (lineLength - 1))
    || isSymbolAt list (Num.subWrap index (lineLength + 1))

isSymbolAt = \list, index ->
    when List.get list index is
        Err _ -> Bool.false
        Ok d -> isSymbol d

isSymbol = \c ->
    (c < '0' || c > '9') && c != '.' && c != '\n'

isDigit = \c ->
    c >= '0' && c <= '9'

addToSum = \state ->
    { state & sum: state.sum + state.currentNumber, shouldAdd: Bool.false, currentNumber: 0 }

example : Str
example =
    """
    467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598..
    """

unwrap = \res ->
    when res is
        Ok x -> x
        Err _ -> crash "bad unwrap"
