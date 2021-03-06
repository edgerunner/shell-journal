module Utilities exposing (anyOf, applySecond, clip, combineWith, ensure, handleError, only, optionalString, shoehorn, splat2, splat3)


handleError : (err -> ok) -> Result err ok -> ok
handleError handle result =
    case result of
        Err err ->
            handle err

        Ok ok ->
            ok


only : (a -> Bool) -> a -> Maybe a
only predicate value =
    if predicate value then
        Just value

    else
        Nothing


ensure : (a -> Bool) -> e -> a -> Result e a
ensure predicate error value =
    if predicate value then
        Ok value

    else
        Err error


optionalString : String -> Bool -> String
optionalString string condition =
    if condition then
        string

    else
        ""


combineWith : (a -> b -> c) -> ( a, b ) -> c
combineWith fn ( a, b ) =
    fn a b


clip : (a -> Bool) -> Int -> List a -> List a
clip =
    clip_ []


clip_ : List a -> (a -> Bool) -> Int -> List a -> List a
clip_ clippings predicate range lines =
    case lines of
        [] ->
            clippings
                |> List.take (range * 2)
                |> List.reverse

        first :: rest ->
            if predicate first then
                ( clippings, rest )
                    |> Tuple.mapFirst
                        (List.take range >> List.reverse)
                    |> Tuple.mapSecond
                        (List.take range >> (::) first)
                    |> combineWith (++)

            else
                clip_
                    (first :: clippings)
                    predicate
                    range
                    rest


applySecond : ( a -> b, a ) -> b
applySecond ( func, val ) =
    func val


splat2 : (x -> a) -> (x -> b) -> x -> ( a, b )
splat2 mapA mapB x =
    ( mapA x, mapB x )


splat3 : (x -> a) -> (x -> b) -> (x -> c) -> x -> ( a, b, c )
splat3 mapA mapB mapC x =
    ( mapA x, mapB x, mapC x )


anyOf : List (subject -> Bool) -> subject -> Bool
anyOf predicates subject =
    List.foldl ((|>) subject >> (||)) False predicates


shoehorn : (a -> b -> c) -> b -> a -> c
shoehorn fn b a =
    fn a b
