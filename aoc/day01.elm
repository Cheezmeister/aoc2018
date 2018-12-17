import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, style)
import Dict exposing (Dict)
import Time
import Set exposing (Set)

import Http

import AoC exposing (..)

setJoin delim = String.join delim << Set.toList

-- ¯\_(ツ)_/¯
giveMeTheDamnThing : Maybe Int -> Int
giveMeTheDamnThing x =
  case x of
    Just y -> y
    Nothing -> 8675309

processInput : String -> List Int
processInput i = i
  |> String.trim
  |> String.split "\n"
  |> List.map String.toInt
  |> List.map giveMeTheDamnThing

main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


-- MODEL

type alias Model =
  { f : Int
  , baseInput : String
  , currentInput : List Int
  , seen : Set Int
  , answer : String
  }

init : () -> (Model, Cmd Msg)
init _ = (softInit "1", AoC.fetchInputForDay 1 FetchedInput)

softInit input =
   { f = 0
   , baseInput = input
   , currentInput = processInput input
   , seen = Set.empty
   , answer = "???"
   }


-- UPDATE

type Msg
  = Tick Time.Posix
  | Input String
  | FetchedInput (Result Http.Error String)

lockAnswer f model =
  if Set.member f model.seen && model.answer == "???"
  then String.fromInt f
  else model.answer

step : Model -> Model
step model =
  case model.currentInput of
    [] ->
      { model | currentInput = processInput model.baseInput }
    shift :: tail ->
      let f = model.f + shift
          answer = lockAnswer f model in
      { model
              | f = f
              , seen = Set.insert f model.seen
              , currentInput = tail
              , answer = answer
      }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Input newInput ->
      (softInit newInput, Cmd.none)
    FetchedInput (Ok newInput) ->
      (softInit newInput, Cmd.none)
    FetchedInput (Err _) ->
      (softInit "There was a http error", Cmd.none)
    Tick _ ->
      (step model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1 Tick -- Update every 1ms so input cycles ~once per second

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ text "Input: "
    , viewInput model.baseInput Input
    , div []
      [ text "Frequency: "
      , span (AoC.informational) [ text (String.fromInt model.f) ]
      ]
    , text "Part 1: "
    , div (AoC.output) [ text (List.foldl (+) 0 (processInput model.baseInput) |> String.fromInt) ]
    , text "Part 2: "
    , div (AoC.output) [ text model.answer ]
    ]
