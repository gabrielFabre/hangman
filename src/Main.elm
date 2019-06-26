module Main exposing (generateRandomIndex, hasWon, main)

import Api
import Array
import Browser
import Browser.Events
import Css exposing (..)
import Html.Styled exposing (Html, button, div, img, input, text, toUnstyled)
import Html.Styled.Attributes exposing (css, src, value)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Json.Decode as D exposing (Decoder)
import Pendu exposing (Letter, reveal, updatecounter)
import Random



-- model


type Page
    = Home
    | Game Model
    | Input String


type alias Model =
    { word : Maybe (List Letter)
    , counter : Int
    , triedChars : List Char
    , words : List String
    }


init : () -> ( Page, Cmd Msg )
init _ =
    ( Home, Cmd.none )


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



-- update


type Msg
    = OnClickSolo
    | OnClickInput
    | OnClickKey Char
    | GotWords (Result Http.Error (List String))
    | RandomInt Int
    | OnKeyPressed Char
    | OnClickReplay
    | OnInput String
    | OnClickValid
    | OnClickHome


update : Msg -> Page -> ( Page, Cmd Msg )
update msg page =
    let
        _ =
            Debug.log "msg" msg
    in
    case ( page, msg ) of
        ( Home, OnClickSolo ) ->
            ( Game
                { word = Nothing
                , counter = 10
                , triedChars = []
                , words = []
                }
            , Api.words GotWords
            )

        ( Home, OnClickInput ) ->
            ( Input "", Cmd.none )

        ( Game model, OnClickKey char ) ->
            ( Game (updateModel model char), Cmd.none )

        ( Game model, GotWords (Ok words) ) ->
            ( Game
                { model
                    | words = words
                }
            , generateRandomIndex words
            )

        ( Game model, GotWords (Err error) ) ->
            ( Game model, Cmd.none )

        ( Game model, RandomInt int ) ->
            ( Game { model | word = Pendu.pickWord int model.words }, Cmd.none )

        ( Game model, OnKeyPressed char ) ->
            case state model of
                Playing _ ->
                    ( Game (updateModel model char), Cmd.none )

                _ ->
                    ( Game model, Cmd.none )

        ( Game model, OnClickReplay ) ->
            if List.isEmpty model.words then
                ( Input "", Cmd.none )

            else
                ( Game { model | word = Nothing, counter = 10, triedChars = [] }
                , generateRandomIndex model.words
                )

        ( Input _, OnInput input ) ->
            case isInputGood input of
                Just validString ->
                    ( Input validString, Cmd.none )

                Nothing ->
                    ( page, Cmd.none )

        ( Input string, OnClickValid ) ->
            ( Game
                { word = Just (Pendu.simple (String.toUpper string))
                , counter = 10
                , triedChars = []
                , words = []
                }
            , Cmd.none
            )

        ( Game model, onClickHome ) ->
            ( Home, Cmd.none )

        _ ->
            ( page, Cmd.none )


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


generateRandomIndex : List String -> Cmd Msg
generateRandomIndex list =
    Random.generate RandomInt (Random.int 0 (List.length list - 1))


isInputGood : String -> Maybe String
isInputGood input =
    if List.all Char.isAlpha (String.toList input) && String.length input <= 15 then
        Just input

    else
        Nothing



-- view


view : Page -> Html Msg
view page =
    let
        attributes =
            [ flexGrow (int 1)
            , displayFlex
            , flexDirection column
            , justifyContent center
            , alignItems center
            ]
    in
    case page of
        Home ->
            div [ css attributes ]
                [ div
                    [ css
                        [ fontSize (px 40)
                        ]
                    ]
                    [ text "Home" ]
                , div
                    [ css
                        [ marginTop (px 20) ]
                    ]
                    [ text "Choisissez un mode de jeu :" ]
                , div
                    [ css
                        [ margin (px 20)
                        ]
                    ]
                    [ button
                        [ onClick OnClickSolo
                        , css
                            [ padding (px 10)
                            , margin (px 20)
                            , fontSize
                                (px 30)
                            , color (rgb 148 99 71)
                            , paddingLeft (px 40)
                            , paddingRight (px 40)
                            ]
                        ]
                        [ text "Solo" ]
                    ]
                , div []
                    [ button
                        [ onClick OnClickInput
                        , css
                            [ color (rgb 148 99 71)
                            , fontSize (px 30)
                            , padding (px 10)
                            , margin (px 20)
                            ]
                        ]
                        [ text "2 joueurs" ]
                    ]
                ]

        Game model ->
            case state model of
                Initializing ->
                    div [] []

                Won word ->
                    div [ css attributes ]
                        [ wordView word
                        , imageView model.counter
                        , div [] [ text "Vous avez gagné !" ]
                        , buttonReplay
                        , buttonHome
                        ]

                Lost word ->
                    div [ css attributes ]
                        [ wordView word
                        , imageView model.counter
                        , div [] [ text "Vous avez perdu !" ]
                        , buttonReplay
                        , buttonHome
                        ]

                Playing word ->
                    div
                        [ css attributes
                        ]
                        [ wordView word
                        , imageView model.counter
                        , keyboard model.triedChars
                        ]

        Input string ->
            div [ css attributes ]
                [ div
                    [ css
                        [ margin (px 10)
                        , marginBottom (px 10)
                        , fontSize (px 20)
                        ]
                    ]
                    [ text "Joueur A : choisissez un mot à faire deviner au joueur B : " ]
                , input
                    [ css
                        [ marginTop (px 30)
                        , marginBottom (px 10)
                        ]
                    , onInput OnInput
                    , value string
                    ]
                    []
                , div []
                    [ button
                        [ onClick OnClickValid
                        , css
                            [ marginTop (px 15)
                            , marginBottom (px 15)
                            , fontSize (px 18)
                            , padding (px 5)
                            , backgroundColor (rgb 57 179 112)
                            , color (rgb 0 0 0)
                            , borderRadius (px 10)
                            , border (px 0)
                            , disabled
                                [ backgroundColor (rgb 200 200 200) ]
                            ]
                        , Html.Styled.Attributes.disabled (String.length string < 3)
                        ]
                        [ text "Valider" ]
                    ]
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


imageView : Int -> Html msg
imageView counter =
    img [ src ("images/step" ++ String.fromInt counter ++ ".svg") ] []


buttonHome : Html Msg
buttonHome =
    div
        [ css
            [ borderRadius (px 5)
            , marginTop (rem 2)
            , fontSize (px 12)
            ]
        ]
        [ button
            [ onClick OnClickHome
            , css
                [ color (rgb 255 255 255)
                , backgroundColor (rgb 255 165 0)
                , borderRadius (px 7)
                , padding2 (px 10) (px 15)
                , border zero
                , paddingLeft (px 25)
                , paddingRight (px 25)
                ]
            ]
            [ text "Home" ]
        ]


buttonReplay : Html Msg
buttonReplay =
    div
        [ css
            [ borderRadius (px 5)
            , marginTop (rem 5)
            , fontSize (px 12)
            ]
        ]
        [ button
            [ onClick OnClickReplay
            , css
                [ color (rgb 255 255 255)
                , backgroundColor (rgb 255 51 51)
                , borderRadius (px 7)
                , padding2 (px 10) (px 15)
                , border zero
                ]
            ]
            [ text "Rejouer !" ]
        ]



--format


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



-- main


main : Program () Page Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = \page -> Browser.Events.onKeyPress keyDecoder
        }


keyDecoder : Decoder Msg
keyDecoder =
    D.field "key" D.string
        |> D.andThen
            (\string ->
                case String.uncons string of
                    Just ( char, "" ) ->
                        if Char.isAlpha char then
                            D.succeed (OnKeyPressed (Char.toUpper char))

                        else
                            D.fail "failed to decode letter char"

                    _ ->
                        D.fail "failed to decode char"
            )
