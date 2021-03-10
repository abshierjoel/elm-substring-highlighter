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


type ValidWord
    = Numeric String
    | Alphabetic String (Maybe String)


validWordToString : ValidWord -> String
validWordToString vw =
    case vw of
        Numeric word ->
            word

        Alphabetic word contraction_ ->
            contraction_
                |> Maybe.withDefault ""
                |> (++) word


apostrophe : Char
apostrophe =
    Char.fromCode 39


isIgnorable : Char -> Bool
isIgnorable =
    not << Char.isAlphaNum


numericWord : Parser String
numericWord =
    succeed Numeric
        |. chompWhile isIgnorable
        |= (getChompedString <|
                succeed identity
                    |. chompIf Char.isDigit
                    |. chompWhile Char.isDigit
           )
        --|. chompIf isIgnorable
        |. chompWhile isIgnorable


contraction : Parser (Maybe String)
contraction =
    (getChompedString <|
        succeed identity
            |. chompIf ((==) apostrophe)
            |. chompIf Char.isAlpha
            |. chompWhile Char.isAlpha
    )
        |> Parser.map Just


alphabeticWord : Parser String
alphabeticWord =
    succeed Alphabetic
        |. chompWhile isIgnorable
        |= (getChompedString <|
                succeed identity
                    |. chompIf Char.isAlphaNum
                    |. chompWhile Char.isAlpha
           )
        |= oneOf [ backtrackable contraction, succeed Nothing ]
        --|. chompIf isIgnorable
        |. chompWhile isIgnorable


matchHelper : String -> Parser (Step (List String) (List String))
matchHelper match =
    oneOf
        [ succeed (\vw -> Loop (vw :: match))
            |= oneOf [ backtrackable alphabeticWord, backtrackable numericWord ]
        , succeed () |> Parser.map (\_ -> Done (List.reverse match))
        ]


parseString : String -> Parser (List String)
parseString match =
    loop [] matchHelper
        |> Parser.map (String.toLower match)


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
