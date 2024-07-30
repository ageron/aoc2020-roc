app [main] { cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.12.0/Lb8EgiejTUzbggO2HVVuPJFkwvvsfW6LojkLR20kTVE.tar.br" }

import cli.Stdout
import cli.Task exposing [Task]
import cli.Arg
import cli.File
import cli.Utc

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25

dataPath = \day ->
    "data/day$(if day < 10 then "0" else "")$(Num.toStr day).txt"

loadData = \day -> day |> dataPath |> File.readUtf8!

solutions = [
    (Day01.part1, Day01.part2),
    (Day02.part1, Day02.part2),
    (Day03.part1, Day03.part2),
    (Day04.part1, Day04.part2),
    (Day05.part1, Day05.part2),
    (Day06.part1, Day06.part2),
    (Day07.part1, Day07.part2),
    (Day08.part1, Day08.part2),
    (Day09.part1, Day09.part2),
    (Day10.part1, Day10.part2),
    (Day11.part1, Day11.part2),
    (Day12.part1, Day12.part2),
    (Day13.part1, Day13.part2),
    (Day14.part1, Day14.part2),
    (Day15.part1, Day15.part2),
    (Day16.part1, Day16.part2),
    (Day17.part1, Day17.part2),
    (Day18.part1, Day18.part2),
    (Day19.part1, Day19.part2),
    (Day20.part1, Day20.part2),
    (Day21.part1, Day21.part2),
    (Day22.part1, Day22.part2),
    (Day23.part1, Day23.part2),
    (Day24.part1, Day24.part2),
    (Day25.part1, Day25.part2),
]

runSolution = \solution, index, input ->
    Stdout.write! "Part $(Num.toStr index): "
    startTime = Utc.now!
    result = solution input
    endTime = Utc.now!
    delta = Utc.deltaAsMillis startTime endTime |> Num.toStr
    Stdout.line! "$(result) ($(delta)ms)"

checkDay = \day ->
    if day < 1 || day > 25 then Task.err (InvalidDay "Must be between 1 and 25") else Task.ok {}

runDay = \dayArg ->
    day = Str.toU64 dayArg |> Task.fromResult!
    checkDay! day
    Stdout.line! "----- Day $(Num.toStr day) -----"
    input = loadData! day
    (part1, part2) = List.get solutions (day - 1) |> Task.fromResult!
    runSolution! part1 1 input
    runSolution! part2 2 input

sequence = \taskList ->
    Task.loop (taskList, List.withCapacity (List.len taskList)) \(tasks, values) ->
        when tasks is
            [task, .. as rest] ->
                value = task!
                Task.ok (Step (rest, List.append values value))

            [] ->
                Task.ok (Done values)

main =
    args = Arg.list! {}
    daysStr = if List.len args < 2 then List.range { start: At 1, end: At 25 } |> List.map Num.toStr else args |> List.dropFirst 1
    _ = daysStr |> List.map runDay |> sequence!
    Task.ok {}

