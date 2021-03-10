module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input
import Element.Region as Region
import Html exposing (Html, h1, input)
import Parser exposing (..)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }



---- MODEL ----


type alias Model =
    { body : String
    , input : String
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


initialModel =
    { body = "This is some text that I want to understand the quality of very crazy zippy-zappy, bold, letters and is nice."
    , input = ""
    }



---- UPDATE ----


type Msg
    = TextChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextChanged newText ->
            ( { model | input = newText }, Cmd.none )



---- PARSE ----


matcher : ( List Char, String ) -> Parser String
matcher ( key, body ) =
    succeed ()
        |. chompWhile (\c -> c /= maybeChar (List.head key))
        |> Parser.getChompedString


maybeChar : Maybe Char -> Char
maybeChar char =
    case char of
        Just c ->
            c

        Nothing ->
            ' '



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        parsedString =
            run <| matcher ( String.toList model.input, model.body )

        result =
            case parsedString model.body of
                Ok res ->
                    res

                _ ->
                    "Failure"
    in
    layout [] <|
        column [ width fill, paddingXY 500 50, spacing 20 ]
            [ row [ width fill ]
                [ el [ Region.heading 1, centerX, Font.size 40 ] (text "Substring Matcher") ]
            , row [ width fill, Font.alignLeft ]
                [ Element.Input.text []
                    { label = Element.Input.labelLeft [] (text "Key: ")
                    , onChange = TextChanged
                    , placeholder = Nothing
                    , text = model.input
                    }
                ]
            , row
                [ width fill
                , paddingXY 40 20
                , Background.color (rgb 0.9 0.9 0.9)
                , Border.color (rgb 0.8 0.8 0.8)
                , Border.width 1
                , Border.rounded 5
                ]
                [ paragraph [ Font.italic, Font.size 14, Font.color (rgb 0.2 0.2 0.2) ] [ text model.body ] ]
            , row [ width fill ]
                [ paragraph [] [ text result ] ]
            ]
