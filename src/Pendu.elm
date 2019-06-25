module Pendu exposing (Letter, change, contains, foo, pickWord, reveal, see, simple, updatecounter)

import Array


contains : String -> Char -> Bool
contains word char =
    word
        |> String.toList
        |> List.member char


type alias Letter =
    { char : Char
    , isGuessed : Bool
    }


foo : Char -> Letter
foo c =
    { char = c
    , isGuessed = False
    }


see : Char -> List Letter -> Bool
see c list =
    list
        |> List.map .char
        |> List.member c


change : Char -> Letter -> Letter
change char letter =
    if letter.char == char then
        { letter | isGuessed = True }

    else
        letter


simple : String -> List Letter
simple string =
    string
        |> String.toList
        |> List.map foo


reveal : Char -> List Letter -> List Letter
reveal char list =
    list
        |> List.map (change char)


updatecounter : Char -> List Letter -> List Char -> Int -> Int
updatecounter char letters triedChars counter =
    if see char letters then
        counter

    else if List.member char triedChars then
        counter

    else
        counter - 1


pickWord : Int -> List String -> Maybe (List Letter)
pickWord int list =
    list
        |> Array.fromList
        |> Array.get int
        |> Maybe.map simple
