module [part1, part2]

combinations = \list, n ->
    if n == 0 || List.len list < n then
        []
    else if n == 1 then
        List.map list \v -> [v]
    else
        when list is
            [x, .. as rest] ->
                with = combinations rest (n - 1) |> List.map \combi -> List.prepend combi x
                without = combinations rest n
                List.concat with without

            _ -> crash "Unreachable"

findExpensesProduct = \input, n ->
    input
    |> Str.split "\n"
    |> List.keepOks (\val -> Str.toI32 val)
    |> combinations n
    |> List.keepIf \expenses -> List.sum expenses == 2020
    |> List.map \list -> list |> List.product |> Num.toStr
    |> List.first
    |> Result.withDefault "Invalid input"

part1 = \input -> findExpensesProduct input 2

part2 = \input -> findExpensesProduct input 3
