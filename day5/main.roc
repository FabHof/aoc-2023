app "AoC"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }

    imports [pf.Stdout, pf.Task.{ Task }, "input.txt" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    Stdout.line "P1: \(part1 input |> Num.toStr) P2: \(part2 input |> Num.toStr)"

part1 = \str ->
    { seeds, mappings } = parse str
    List.map seeds (\seed -> doMappings seed mappings)
    |> List.min
    |> unwrap

doMappings = \seed, mappings ->
    List.walk mappings seed doMapping

doMapping = \seed, mapping ->
    matchesSeed = \map -> seed >= map.source && seed < map.source + map.range
    matchingMap = List.findFirst mapping matchesSeed
    when matchingMap is
        Err NotFound -> seed
        Ok map -> seed + map.destination - map.source

part2 = \str ->
    { seeds, mappings } = parse str
    seedRanges =
        seeds
        |> List.chunksOf 2
        |> List.map
            \l -> { start: List.first l |> unwrap, range: List.last l |> unwrap }
    List.map seedRanges (\s -> doMappings2 s mappings)
    |> List.min
    |> unwrap

doMappings2 = \seedRange, mappings ->
    when List.first mappings is
        Ok mapping ->
            doMapping2 seedRange mapping
            |> List.map \sr -> doMappings2 sr (List.dropAt mappings 0)
            |> List.min
            |> unwrap

        _ -> seedRange.start

doMapping2 = \seedRange, mapping ->
    matchesSeed = \map -> seedRange.start >= map.source && seedRange.start < map.source + map.range
    isBigger = \map -> map.source > seedRange.start + seedRange.range - 1
    matchingMap = List.findFirst mapping matchesSeed
    when matchingMap is
        Err _ ->
            nextMap =
                List.keepIf mapping isBigger
                |> List.sortWith \m1, m2 -> Num.compare m1.source m2.source
                |> List.first
            when nextMap is
                Err _ -> [seedRange]
                Ok next ->
                    if next.source > seedRange.start + seedRange.range - 1 then
                        [seedRange]
                    else
                        List.append
                            (
                                doMapping2
                                    {
                                        start: next.source,
                                        range: seedRange.range + seedRange.start - next.source,
                                    }
                                    mapping
                            )
                            { seedRange & range: next.source - seedRange.start }

        Ok map ->
            newStart = seedRange.start + map.destination - map.source
            if seedRange.start + seedRange.range > map.source + map.range then
                List.append
                    (
                        doMapping2
                            {
                                start: map.source + map.range,
                                range: seedRange.range + seedRange.start - map.source - map.range,
                            }
                            mapping
                    )
                    { start: newStart, range: map.range + map.source - seedRange.start }
            else
                [{ seedRange & start: newStart }]

parse = \str ->

    toNumArray = \s ->
        Str.split s " "
        |> List.keepOks Str.toNat

    toMap = \s ->
        list = toNumArray s
        {
            destination: List.get list 0 |> unwrap,
            source: List.get list 1 |> unwrap,
            range: List.get list 2 |> unwrap,
        }

    parts = Str.split str "\n\n"

    seeds =
        List.first parts
        |> unwrap
        |> Str.splitFirst ": "
        |> unwrap
        |> .after
        |> toNumArray

    mappings =
        List.dropAt parts 0
        |> List.map \map ->
            Str.split map "\n"
            |> List.dropAt 0
            |> List.map toMap

    { seeds: seeds, mappings: mappings }

example : Str
example =
    """
    seeds: 79 14

    seed-to-soil map:
    50 98 2
    52 50 48

    soil-to-fertilizer map:
    0 15 37
    37 52 2
    39 0 15

    fertilizer-to-water map:
    49 53 8
    0 11 42
    42 0 7
    57 7 4

    water-to-light map:
    88 18 7
    18 25 70

    light-to-temperature map:
    45 77 23
    81 45 19
    68 64 13

    temperature-to-humidity map:
    0 69 1
    1 0 69

    humidity-to-location map:
    60 56 37
    56 93 4  
    """

unwrap = \res ->
    when res is
        Ok x -> x
        Err _ -> crash "bad unwrap"
