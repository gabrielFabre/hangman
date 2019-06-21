module Main exposing (hasWon, main)

import Browser
import Css exposing (border2, height, px, solid, width)
import Html.Styled exposing (Html, button, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Pendu exposing (Letter, reveal, updatecounter)


type alias Model =
    { word : List Letter
    , counter : Int
    , triedChars : List Char
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { word = Pendu.simple "PHOENIX"
      , counter = 10
      , triedChars = []
      }
    , Cmd.none
    )


type State
    = Playing
    | Lost
    | Won


state : Model -> State
state model =
    if model.counter == 0 then
        Lost

    else if hasWon model.word then
        Won

    else
        Playing


type Msg
    = OnClickKey Char


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnClickKey char ->
            ( updateModel model char, Cmd.none )


updateModel : Model -> Char -> Model
updateModel model char =
    { model
        | word = reveal char model.word
        , counter = updatecounter char model.word model.triedChars model.counter
        , triedChars = char :: model.triedChars
    }


view : Model -> Html Msg
view model =
    case state model of
        Won ->
            div []
                [ div [] [ text (format model.word) ]
                , div [] [ text (String.fromInt model.counter) ]
                , div [] [ text "Vous avez gagné !" ]
                ]

        Lost ->
            div []
                [ div [] [ text (format model.word) ]
                , div [] [ text (String.fromInt model.counter) ]
                , div [] [ text "Vous avez perdu !" ]
                ]

        Playing ->
            div []
                [ div [] [ text (format model.word) ]
                , div [] [ text (String.fromInt model.counter) ]
                , keyboard
                , case lastRepeatedChar model.triedChars of
                    Just char ->
                        text ("Vous avez déjà essayé la lettre " ++ String.fromChar char)

                    Nothing ->
                        text ""
                ]


hasWon : List Letter -> Bool
hasWon list =
    List.all .isGuessed list


key : Char -> Html Msg
key char =
    button [ onClick (OnClickKey char) ] [ text (String.fromChar char) ]


keyboard : Html Msg
keyboard =
    div []
        (List.range 65 90
            |> List.map Char.fromCode
            |> List.map key
        )


format : List Letter -> String
format list =
    list
        |> List.map formatLetter
        |> List.map String.fromChar
        |> String.join " "


formatLetter : Letter -> Char
formatLetter letter =
    if letter.isGuessed then
        letter.char

    else
        '_'


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }


lastRepeatedChar : List Char -> Maybe Char
lastRepeatedChar list =
    case list of
        head :: tail ->
            if List.member head tail then
                Just head

            else
                Nothing

        [] ->
            Nothing
