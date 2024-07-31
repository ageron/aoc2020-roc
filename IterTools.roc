module [combinations]

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
