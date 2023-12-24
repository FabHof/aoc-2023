app "AoC"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }

    imports [pf.Stdout, pf.Task.{ Task }, "input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =

    dbg part1 input

    dbg part2 input

    Stdout.line "Done"

part1 = \str ->
    split = Str.split str "\n\n"
    workflow = split |> List.first |> unwrap |> parseWorkflow
    parts = split |> List.last |> unwrap |> parseParts
    List.keepIf parts \p -> runWorkflow workflow p
    |> List.map sumXMAS
    |> List.sum

part2 = \str ->
    split = Str.split str "\n\n"
    workflow = split |> List.first |> unwrap |> parseWorkflow
    fullRatingRange = {
        min: 1,
        max: 4000,
    }
    fullPartRange = {
        x: fullRatingRange,
        m: fullRatingRange,
        a: fullRatingRange,
        s: fullRatingRange,
    }
    toDo =
        List.findFirst workflow \w ->
            w.name == "in"
        |> unwrap
    doSplits fullPartRange toDo workflow
    |> List.map calcCombinations
    |> List.sum

calcCombinations = \{ x, m, a, s } ->
    (x.max - x.min + 1)
    * (m.max - m.min + 1)
    * (a.max - a.min + 1)
    *
    (s.max - s.min + 1)

doSplits = \partRange, toDo, workflow ->
    startState = {
        lastOutRange: Ok partRange,
        inList: [],
    }
    conditionResult = List.walkUntil toDo.conditions startState \state, condition ->
        (in, out) = doSplitCheck condition.check (state.lastOutRange |> unwrap)
        newInList =
            when in is
                One inRange -> List.append state.inList (condition.target, inRange)
                Nothing -> state.inList
        newState = { state & inList: newInList }
        when out is
            One outRange -> Continue { newState & lastOutRange: Ok outRange }
            Nothing -> Break { newState & lastOutRange: Err Empty }
    targetReached =
        List.keepIf conditionResult.inList \(target, _) ->
            target == "A"
        |> List.map .1
        |> List.concat
            (
                if toDo.defaultTarget == "A" && Result.isOk conditionResult.lastOutRange then
                    [conditionResult.lastOutRange |> unwrap]
                else
                    [])

    nextList =
        List.keepIf conditionResult.inList \(target, _) ->
            target != "A" && target != "R"
        |> List.concat
            (
                if toDo.defaultTarget != "A" && toDo.defaultTarget != "R" && Result.isOk conditionResult.lastOutRange then
                    [(toDo.defaultTarget, conditionResult.lastOutRange |> unwrap)]
                else
                    [])
    List.joinMap nextList \(target, range) ->
        nextToDo =
            List.findFirst workflow \w ->
                w.name == target
            |> unwrap
        doSplits range nextToDo workflow
    |> List.concat targetReached

doSplitCheck = \check, partRange ->
    ratingRange = partRange |> check.category
    (inRange, outRange) =
        if ratingRange.min |> check.operation check.number then
            # min is in
            if ratingRange.max |> check.operation check.number then
                # max is also in
                (One ratingRange, Nothing)
            else
                # min in, max out: condition has to be LT
                inMax = check.number - 1
                outMin = check.number
                in = { ratingRange & max: inMax }
                out = { ratingRange & min: outMin }
                (One in, One out)
        else if ratingRange.max |> check.operation check.number then
            # min out, max in: condition has to be GT
            outMax = check.number
            inMin = check.number + 1
            out = { ratingRange & max: outMax }
            in = { ratingRange & min: inMin }
            (One in, One out)
        else
            # max is also out
            (Nothing, One ratingRange)
    (inRange |> addToPart partRange check.categoryName, outRange |> addToPart partRange check.categoryName)

addToPart = \ratingRange, part, categoryName ->
    when ratingRange is
        One range ->
            when categoryName is
                'x' -> One { part & x: range }
                'm' -> One { part & m: range }
                'a' -> One { part & a: range }
                's' -> One { part & s: range }
                _ -> crash "Still no XMAS"

        Nothing -> Nothing

sumXMAS = \{ x, m, a, s } ->
    x + m + a + s

runWorkflow = \workflow, part ->
    toDo =
        List.findFirst workflow \w ->
            w.name == "in"
        |> unwrap
    doWorkflow workflow toDo part

doWorkflow = \workflow, toDo, part ->
    validCondition = List.findFirst toDo.conditions \condition -> doCheck condition.check part
    next =
        when validCondition is
            Ok condition -> condition.target
            Err _ -> toDo.defaultTarget
    when next is
        "A" -> Bool.true
        "R" -> Bool.false
        _ ->
            nextToDo =
                List.findFirst workflow \w ->
                    w.name == next
                |> unwrap
            doWorkflow workflow nextToDo part

doCheck = \check, part ->
    part |> check.category |> check.operation check.number

parseWorkflow = \str ->
    line <- Str.split str "\n" |> List.map Str.toUtf8 |> List.map
    name = List.splitFirst line '{' |> unwrap |> .before |> Str.fromUtf8 |> unwrap
    conditionStrings =
        List.dropFirst line (Str.countUtf8Bytes name + 1)
        |> List.dropLast 1
        |> Str.fromUtf8
        |> unwrap
        |> Str.split ","
    defaultTarget = List.last conditionStrings |> unwrap
    conditions =
        condition <- conditionStrings |> List.dropLast 1 |> List.map
        split = Str.split condition ":"
        target = List.last split |> unwrap
        check =
            when List.first split |> unwrap |> Str.toUtf8 is
                [cat, op, .. as num] ->
                    {
                        category: toCat cat,
                        categoryName: cat,
                        operation: toOp op,
                        number: toNum num,
                    }

                _ -> crash "not a real check"
        { check, target }

    { name, conditions, defaultTarget }

toCat = \cat ->
    when cat is
        'x' -> .x
        'm' -> .m
        'a' -> .a
        's' -> .s
        _ -> crash "No XMAS"

toOp = \op ->
    when op is
        '>' -> Num.isGt
        '<' -> Num.isLt
        _ -> crash "No operation"

toNum = \num ->
    Str.fromUtf8 num |> unwrap |> Str.toU64 |> unwrap

parseParts = \str ->
    line <- Str.split str "\n" |> List.map
    part =
        category <- Str.split line "," |> List.map
        Str.toUtf8 category
        |> List.keepIf \c ->
            c >= '0' && c <= '9'
        |> Str.fromUtf8
        |> unwrap
        |> Str.toU64
        |> unwrap
    when part is
        [x, m, a, s] -> { x, m, a, s }
        _ -> crash "No xmas!"

example =
    """
    px{a<2006:qkq,m>2090:A,rfg}
    pv{a>1716:R,A}
    lnx{m>1548:A,A}
    rfg{s<537:gd,x>2440:R,A}
    qs{s>3448:A,lnx}
    qkq{x<1416:A,crn}
    crn{x>2662:A,R}
    in{s<1351:px,qqz}
    qqz{s>2770:qs,m<1801:hdj,R}
    gd{a>3333:R,R}
    hdj{m>838:A,pv}

    {x=787,m=2655,a=1222,s=2876}
    {x=1679,m=44,a=2067,s=496}
    {x=2036,m=264,a=79,s=2244}
    {x=2461,m=1339,a=466,s=291}
    {x=2127,m=1623,a=2188,s=1013}
    """

unwrap = \res ->
    when res is
        Ok x -> x
        Err _ ->
            crash "bad unwrap"

debug = \x ->
    dbg x

    x
