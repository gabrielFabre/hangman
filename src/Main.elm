module Main exposing (generateRandomIndex, hasWon, main)

import Api
import Array
import Browser
import Css exposing (..)
import Html.Styled exposing (Html, button, div, img, text, toUnstyled)
import Html.Styled.Attributes exposing (css, src)
import Html.Styled.Events exposing (onClick)
import Http
import Pendu exposing (Letter, reveal, updatecounter)
import Random


type alias Model =
    { word : Maybe (List Letter)
    , counter : Int
    , triedChars : List Char
    , words : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { word = Nothing
      , counter = 10
      , triedChars = []
      , words = []
      }
    , Api.words GotWords
    )


type State
    = Initializing
    | Playing (List Letter)
    | Lost (List Letter)
    | Won (List Letter)


state : Model -> State
state model =
    case model.word of
        Nothing ->
            Initializing

        Just word ->
            if model.counter == 0 then
                Lost word

            else if hasWon word then
                Won word

            else
                Playing word


type Msg
    = OnClickKey Char
    | GotWords (Result Http.Error (List String))
    | RandomInt Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "msg" msg
    in
    case msg of
        OnClickKey char ->
            ( updateModel model char, Cmd.none )

        GotWords (Ok words) ->
            ( { model
                | words = words
              }
            , generateRandomIndex words
            )

        GotWords (Err error) ->
            ( model, Cmd.none )

        RandomInt int ->
            ( { model | word = Pendu.pickWord int model.words }, Cmd.none )


updateModel : Model -> Char -> Model
updateModel model char =
    case model.word of
        Nothing ->
            model

        Just word ->
            { model
                | word = Just (reveal char word)
                , counter = updatecounter char word model.triedChars model.counter
                , triedChars = char :: model.triedChars
            }


view : Model -> Html Msg
view model =
    let
        attributes =
            [ flexGrow (int 1)
            , displayFlex
            , flexDirection column
            , justifyContent center
            , alignItems center
            ]
    in
    case state model of
        Initializing ->
            div [ css [ backgroundColor (rgb 255 0 0) ] ] []

        Won word ->
            div [ css attributes ]
                [ wordView word
                , imageView model.counter
                , div [] [ text "Vous avez gagné !" ]
                ]

        Lost word ->
            div [ css attributes ]
                [ wordView word
                , imageView model.counter
                , div [] [ text "Vous avez perdu !" ]
                ]

        Playing word ->
            div
                [ css attributes
                ]
                [ wordView word
                , imageView model.counter
                , keyboard model.triedChars
                ]


wordView : List Letter -> Html msg
wordView word =
    div
        [ css
            [ marginBottom (rem 5)
            , fontSize (px 48)
            ]
        ]
        [ text (format word) ]


livesView : Int -> Html msg
livesView counter =
    div
        [ css
            [ marginBottom (rem 5)
            , color (rgb 255 0 0)
            , fontSize (px 36)
            ]
        ]
        [ text (String.fromInt counter) ]


hasWon : List Letter -> Bool
hasWon list =
    List.all .isGuessed list


key : Char -> Bool -> Html Msg
key char isDisabled =
    button
        [ onClick (OnClickKey char)
        , css
            [ width (rem 4)
            , height (rem 4)
            , fontSize (px 24)
            , margin (px 2)
            , focus [ outline none ]
            ]
        , css
            (if isDisabled then
                [ color (rgb 200 200 200)
                , backgroundColor (rgb 230 230 230)
                , border3 (px 1) solid (rgb 200 200 200)
                , borderRadius (px 3)
                ]

             else
                []
            )
        ]
        [ text (String.fromChar char) ]


keyboard : List Char -> Html Msg
keyboard triedChars =
    div []
        [ div []
            (List.range 65 77
                |> List.map Char.fromCode
                |> List.map (\c -> key c (List.member c triedChars))
            )
        , div []
            (List.range 78 90
                |> List.map Char.fromCode
                |> List.map (\c -> key c (List.member c triedChars))
            )
        ]


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


formatLost : List Letter -> String
formatLost list =
    list
        |> List.map .char
        |> List.map String.fromChar
        |> String.join " "


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }


generateRandomIndex : List String -> Cmd Msg
generateRandomIndex list =
    Random.generate RandomInt (Random.int 0 (List.length list - 1))


numberfile : Int -> String
numberfile counter =
    "step" ++ String.fromInt counter


imageView : Int -> Html msg
imageView counter =
    img [ src ("images/step" ++ String.fromInt counter ++ ".svg") ] []
