module Pendu exposing (pickWord, updatecounter)

import Array


updatecounter : Char -> List Char -> List Char -> Int -> Int
updatecounter char letters triedChars counter =
    if List.member char letters then
        counter

    else if List.member char triedChars then
        counter

    else
        counter - 1


pickWord : List String -> Int -> Maybe (List Char)
pickWord list int =
    list
        |> Array.fromList
        |> Array.get int
        |> Maybe.map String.toList
