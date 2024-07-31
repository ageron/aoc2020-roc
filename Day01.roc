module [part1, part2]

import IterTools

findExpensesProduct = \input, n ->
    input
    |> Str.split "\n"
    |> List.keepOks (\val -> Str.toI32 val)
    |> IterTools.combinations n
    |> List.keepIf \expenses -> List.sum expenses == 2020
    |> List.map \list -> list |> List.product |> Num.toStr
    |> List.first
    |> Result.withDefault "Invalid input"

part1 = \input -> findExpensesProduct input 2

part2 = \input -> findExpensesProduct input 3
