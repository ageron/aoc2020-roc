module [part1, part2]

import IterTools
import cli.Task exposing [Task]

parseLine = \line ->
    when line |> Str.split ": " is
        [policy, password] ->
            when policy |> Str.split " " is
                [valuesStr, letterStr] ->
                    when valuesStr |> Str.split "-" is
                        [lowStr, highStr] ->
                            low <- Str.toU64 lowStr |> Result.onErr (\_ -> Err (ParsingError "low")) |> Result.try
                            high <- Str.toU64 highStr |> Result.onErr (\_ -> Err (ParsingError "high")) |> Result.try
                            when letterStr |> Str.toUtf8 is
                                [letter] -> Ok (low, high, letter, password |> Str.toUtf8)
                                _ -> Err (ParsingError "Invalid letter format")

                        _ -> Err (ParsingError "Invalid values format")

                _ -> Err (ParsingError "Invalid policy format")

        _ -> Err (ParsingError "Invalid line format")

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
