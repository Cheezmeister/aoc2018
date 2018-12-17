import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Input5 exposing (input)

testInput = "dabAcCaCBAcCcaDA"


-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , tempInput : String
  }


init : Model
init =
  Model "" "" "" input



-- UPDATE


type Msg
  = Name String
  | Input String
  | HttpSuccess String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Input newInput ->
      { model | tempInput = newInput }

    Name name ->
      { model | name = name }

solve : String -> String
solve string =
  let availablePairs = matchingPairs |> List.concatMap (\ (up, low) -> [up, low] )
      replacements = matchingPairs |> List.concatMap (\ (up, low) -> [ String.replace up "", String.replace low "" ] )

  in if availablePairs |> List.any (\p -> String.contains p string) then List.foldl (\ f a -> f a) string replacements |> solve
     else string

pairs c =
  let up = String.toUpper c in
  ( c ++ up, up ++ c )

letters = "abcdefghijklmnopqrstuvwxyz"

matchingPairs =
  letters
  |> String.split ""
  |> List.map pairs


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ text "Input: "
    , viewInput model.tempInput Input
    , viewAnswer model
    ]

viewInput : String -> (String -> msg) -> Html msg
viewInput v toMsg =
  textarea [ onInput toMsg ] [text v]

takeMinimum list =
  case List.minimum list of 
    Just x -> String.fromInt x
    Nothing -> "nothing"

part2attempt model letter =
  model.tempInput 
    |> String.replace letter ""
    |> String.replace (String.toUpper letter) ""
    |> solve
    |> String.length

part1 model = solve model.tempInput
part2 model = String.split "" letters |> List.map (part2attempt model)

viewAnswer : Model -> Html msg
viewAnswer model =
  div []
    [ div [] [ text "Part 1 Result: " ]
    , div [ style "color" "green" ] [ text (part1 model)]
    , div [ style "color" "blue" ] [ text (String.length (solve model.tempInput) |> String.fromInt) ]
    , div [] [ text "Part 2 Result: " ]
    , div [ style "color" "green" ] [ text (part2 model |> List.map String.fromInt |> String.join "\n")]
    , div [ style "color" "blue" ] [ text (part2 model |> takeMinimum)]
    ]
