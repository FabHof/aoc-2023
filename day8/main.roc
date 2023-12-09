app "AoC"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }

    imports [pf.Stdout, pf.Task.{ Task }, "input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    p1 = part1 input
    p2 = part2 input

    Stdout.line "P1: \(p1 |> Num.toStr) P2: \(p2 |> Num.toStr)"

part1 = \str ->
    { instructions, network } = parse str
    initState = { steps: 0, pos: "AAA", done: Bool.false }
    solve (\p -> p == "ZZZ") instructions network initState

part2 = \str ->
    { instructions, network } = parse str
    stepsToGoal =
        Dict.keys network
        |> List.keepIf \s -> Str.endsWith s "A"
        |> List.map (\s -> solve (\p -> Str.endsWith p "Z") instructions network { steps: 0, pos: s, done: Bool.false })
    stepsToGoal |> lcmList

lcmList = \list ->
    when list is
        [] -> 1
        [x] -> x
        [x, .. as rest] -> lcm x (lcmList rest)

lcm = \a, b ->
    (a * b) // (gcd a b)

gcd = \a, b ->
    if b == 0 then
        a
    else
        gcd b (a % b)

parse = \str ->
    { before, after } = str |> Str.splitFirst "\n\n" |> unwrap
    instructions = parseInstructions before
    network = parseNetwork after
    { instructions, network }

solve = \targetCondition, instructions, network, initState ->
    step = \state, elem ->
        newPos =
            when elem is
                Left -> Dict.get network state.pos |> unwrap |> .0
                Right -> Dict.get network state.pos |> unwrap |> .1
        newState = { state & steps: state.steps + 1, pos: newPos }
        if targetCondition newPos then
            Break { newState & done: Bool.true }
        else
            Continue { newState & done: Bool.false }
    walkResult = List.walkUntil instructions initState step
    if walkResult.done then
        walkResult.steps
    else
        solve targetCondition instructions network walkResult

parseInstructions = \str ->
    Str.toUtf8 str
    |> List.map \c ->
        when c is
            'L' -> Left
            'R' -> Right
            _ -> crash "invalid instruction"

parseNetwork = \str ->
    list =
        line <- Str.split str "\n" |> List.map
        { before: from, after } = Str.splitFirst line " = " |> unwrap
        { before: toLeft, after: toRight } =
            Str.replaceFirst after "(" ""
            |> Str.replaceFirst ")" ""
            |> Str.splitFirst ", "
            |> unwrap
        (from, (toLeft, toRight))
    Dict.fromList list

example : Str
example =
    """
    LR

    AAA = (11B, XXX)
    11B = (XXX, ZZZ)
    ZZZ = (11B, XXX)
    22A = (22B, XXX)
    22B = (22C, 22C)
    22C = (22Z, 22Z)
    22Z = (22B, 22B)
    XXX = (XXX, XXX)
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
