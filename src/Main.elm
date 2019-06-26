module Main exposing (main)

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
    = Home (Maybe (List String))
    | Game Model
    | Input String


type alias Model =
    { word : List Letter
    , counter : Int
    , triedChars : List Char
    , mode : Mode
    }


initialSoloModel : List Letter -> ( String, List String ) -> Model
initialSoloModel word ( head, tail ) =
    { word = word
    , counter = 10
    , triedChars = []
    , mode = Solo head tail
    }


type Mode
    = Solo String (List String)
    | Multi


init : () -> ( Page, Cmd Msg )
init _ =
    ( Home Nothing, Api.words GotWords )



-- ( Game
--     { word =
--         [ { char = 'L', isGuessed = False }
--         , { char = 'E', isGuessed = False }
--         , { char = 'B', isGuessed = False }
--         , { char = 'O', isGuessed = True }
--         , { char = 'N', isGuessed = False }
--         , { char = 'C', isGuessed = False }
--         , { char = 'O', isGuessed = True }
--         , { char = 'I', isGuessed = False }
--         , { char = 'N', isGuessed = False }
--         ]
--     , counter = 0
--     , triedChars = [ 'O' ]
--     , mode = Multi
--     }
-- , Cmd.none
-- )


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



-- update


type Msg
    = OnClickSolo
    | OnClickInput
    | OnClickKey Char
    | GotWords (Result Http.Error (List String))
    | RandomWord ( String, List String ) (List Letter)
    | OnKeyPressed Char
    | OnClickReplay
    | OnInput String
    | OnClickValid
    | OnClickHome


update : Msg -> Page -> ( Page, Cmd Msg )
update msg page =
    case ( page, msg ) of
        ( Home _, GotWords (Ok words) ) ->
            ( Home (Just words), Cmd.none )

        ( Home _, GotWords (Err error) ) ->
            ( Home Nothing, Cmd.none )

        ( Home words, OnClickSolo ) ->
            case words of
                Just list ->
                    case list of
                        [] ->
                            ( page, Cmd.none )

                        head :: tail ->
                            ( page, generateRandomWord head tail )

                Nothing ->
                    ( page, Cmd.none )

        ( Home _, RandomWord ( head, tail ) word ) ->
            ( Game (initialSoloModel word ( head, tail ))
            , Cmd.none
            )

        ( Game _, RandomWord ( head, tail ) word ) ->
            ( Game (initialSoloModel word ( head, tail ))
            , Cmd.none
            )

        ( Home _, OnClickInput ) ->
            ( Input "", Cmd.none )

        ( Input _, OnInput input ) ->
            case isInputGood input of
                Just validString ->
                    ( Input validString, Cmd.none )

                Nothing ->
                    ( page, Cmd.none )

        ( Input string, OnClickValid ) ->
            ( Game
                { word = Pendu.simple (String.toUpper string)
                , counter = 10
                , triedChars = []
                , mode = Multi
                }
            , Cmd.none
            )

        ( Game model, OnClickKey char ) ->
            ( Game (updateModel model char), Cmd.none )

        ( Game model, OnKeyPressed char ) ->
            case state model of
                Playing ->
                    ( Game (updateModel model char), Cmd.none )

                _ ->
                    ( Game model, Cmd.none )

        ( Game model, OnClickReplay ) ->
            case model.mode of
                Solo head tail ->
                    ( page, generateRandomWord head tail )

                Multi ->
                    ( Input "", Cmd.none )

        ( Game model, OnClickHome ) ->
            ( Home Nothing, Api.words GotWords )

        _ ->
            ( page, Cmd.none )


updateModel : Model -> Char -> Model
updateModel model char =
    { model
        | word = reveal char model.word
        , counter = updatecounter char model.word model.triedChars model.counter
        , triedChars = char :: model.triedChars
    }


generateRandomWord : String -> List String -> Cmd Msg
generateRandomWord head tail =
    Random.uniform head tail
        |> Random.map Pendu.simple
        |> Random.generate (RandomWord ( head, tail ))


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
        Home words ->
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
                        , Html.Styled.Attributes.disabled (isSoloDisabled words)
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
                Won ->
                    div [ css attributes ]
                        [ wordView model.word
                        , imageView model.counter
                        , div [ css [ height (px 150), displayFlex, alignItems center, flexDirection column ] ]
                            [ div [] [ text "Vous avez gagné !" ]
                            , buttonReplay
                            , buttonHome
                            ]
                        ]

                Lost ->
                    div [ css attributes ]
                        [ wordLostView model.word
                        , imageView model.counter
                        , div [ css [ height (px 150), displayFlex, alignItems center, flexDirection column ] ]
                            [ div [] [ text "Vous avez perdu !" ]
                            , buttonReplay
                            , buttonHome
                            ]
                        ]

                Playing ->
                    div
                        [ css attributes
                        ]
                        [ wordView model.word
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


isSoloDisabled : Maybe (List String) -> Bool
isSoloDisabled words =
    case words of
        Just list ->
            case list of
                [] ->
                    True

                head :: tail ->
                    False

        Nothing ->
            True


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
    div [ css [ height (px 150) ] ]
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


wordView : List Letter -> Html Msg
wordView word =
    div
        [ css
            [ marginBottom (rem 5)
            , fontSize (px 48)
            , displayFlex
            ]
        ]
        (List.map letterView word)


wordLostView : List Letter -> Html Msg
wordLostView letters =
    div
        [ css
            [ marginBottom (rem 5)
            , fontSize (px 48)
            , displayFlex
            ]
        ]
        (List.map letterLostView letters)


letterView : Letter -> Html Msg
letterView letter =
    if letter.isGuessed then
        div [ css [ margin (px 10) ] ] [ text (String.fromChar letter.char) ]

    else
        div [ css [ margin (px 10) ] ] [ text "_" ]


letterLostView : Letter -> Html Msg
letterLostView letter =
    let
        attributes =
            if letter.isGuessed then
                [ margin (px 10) ]

            else
                [ margin (px 10)
                , color (rgb 220 0 0)
                ]
    in
    div [ css attributes ] [ text (String.fromChar letter.char) ]



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
