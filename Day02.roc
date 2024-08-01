module [part1, part2]

import IterTools
import cli.Task exposing [Task]

parseLine = \line ->
    (policy, password) <- line |> Str.split ": " |> IterTools.toPair |> Result.try
    (valuesStr, letterStr) <- policy |> Str.split " " |> IterTools.toPair |> Result.try
    (lowStr, highStr) <- valuesStr |> Str.split "-" |> IterTools.toPair |> Result.try
    low <- Str.toU64 lowStr |> Result.try
    high <- Str.toU64 highStr |> Result.try
    letter <- letterStr |> Str.toUtf8 |> IterTools.toSingle |> Result.try
    Ok (low, high, letter, password |> Str.toUtf8)

parsePasswordPolicy = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> IterTools.expectOks parseLine

isValidPasswordPart1 = \(minCount, maxCount, letter, password) ->
    count = password |> List.countIf (\c -> c == letter)
    count >= minCount && count <= maxCount

isValidPasswordPart2 = \(index1, index2, letter, password) ->
    letter1 = password |> List.get (index1 - 1) |> Result.withDefault 0
    letter2 = password |> List.get (index2 - 1) |> Result.withDefault 0
    (letter1 == letter) != (letter2 == letter)

countValidPasswords = \input, isValid ->
    when input |> parsePasswordPolicy is
        Ok list -> Num.toStr (list |> List.countIf isValid)
        Err ListContainsErr -> "Invalid input"

part1 = \input -> input |> countValidPasswords isValidPasswordPart1

part2 = \input -> input |> countValidPasswords isValidPasswordPart2
