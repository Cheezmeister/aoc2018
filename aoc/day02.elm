import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, style)

import Http
import Time

import AoC exposing (..)

main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


-- MODEL
type alias Model =
  { baseInput : String
  }

init : () -> (Model, Cmd Msg)
init _ = (softInit "1", AoC.fetchInputForDay 2 FetchedInput)

softInit input =
   { baseInput = input
   }

-- UPDATE

type Msg
  = FetchedInput (Result Http.Error String)
  | Input String
  | Tick Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Input newInput ->
      (softInit newInput, Cmd.none)
    FetchedInput (Ok newInput) ->
      (softInit newInput, Cmd.none)
    FetchedInput (Err httpErr) ->
      (softInit (AoC.niceError httpErr), Cmd.none)
    Tick _ ->
      (model, Cmd.none)

-- SUBSCRIPTIONS
subscriptions model =
  Time.every 1000 Tick

-- VIEW
view model =
  div [ style "background" "black"
      , style "color" "grey"
      ]
    [ AoC.sharedHeader 2
    , horizontalRule
    , informationalSection [ ("First bit of input", String.slice 0 5 model.baseInput ) ]
    , horizontalRule
    , outputSection "???" "???"
    , horizontalRule
    , text "Input: "
    , viewInput model.baseInput Input
    ]
