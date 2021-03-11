module Main exposing (..)

import Browser
import Dict exposing (Dict)
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


matchingWord : String -> Parser String
matchingWord key =
    succeed ()
        |. symbol "-"
        |= List.head (String.toList key)


string =
    succeed ()
        |> getChompedString


matchHelper : String -> List String -> Parser (Step (List String) (List String))
matchHelper key list =
    oneOf
        [ succeed (\vw -> Loop (vw :: list))
            |= oneOf
                [ backtrackable <| matchingWord key
                , string
                ]
        , succeed () |> Parser.map (\_ -> Done (List.reverse list))
        ]


parseString : String -> Parser (List String)
parseString key =
    loop [] (matchHelper key)


getWords : String -> String -> List String
getWords key body =
    case Parser.run (parseString key) body of
        Ok tokenList ->
            tokenList

        Err _ ->
            []



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        parsedString =
            run getWords "am" "Ammunition American Party AMT"

        result =
            case parsedString of
                Ok res ->
                    List.map (\x -> text x) res

                _ ->
                    [ text "Failure" ]
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
                [ paragraph [] result ]
            ]
