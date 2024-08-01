module [combinations, cartesianProduct, expectOks, toSingle, toPair, toTriple]

combinations : List a, U64 -> List List a
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

cartesianProduct : List a, List b -> List (a, b)
cartesianProduct = \list1, list2 ->
    List.joinMap list1 \x ->
        List.map list2 \y ->
            (x, y)

expectOks : List a, (a -> Result b *) -> [Err [ListContainsErr], Ok (List b)]
expectOks = \list, func ->
    filtered = List.keepOks list func
    if List.len filtered != List.len list then
        Err ListContainsErr
    else
        Ok filtered

toSingle = \list ->
    when list is
        [a] -> Ok a
        _ -> Err NotASingle

toPair = \list ->
    when list is
        [a, b] -> Ok (a, b)
        _ -> Err NotAPair

toTriple = \list ->
    when list is
        [a, b, c] -> Ok (a, b, c)
        _ -> Err NotATriple
