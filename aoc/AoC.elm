module AoC exposing
  ( informational
  , output
  , horizontalRule
  , viewInput
  , fetchInputForDay
  , niceError
  , informationalSection
  , outputSection
  , sharedHeader
  )

import Browser
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, style, href)

import Http

bg = style "background"
border = style "border"
rounded = style "border-radius" "3px"

informational = [ bg "green", border "1px solid #040", rounded ]
output = [ bg "blue", border "1px solid #004", rounded ]
heading = [style "margin" "0", style "color" "#f40"]

horizontalRule : Html msg
horizontalRule = Html.hr [] []

viewInput : String -> (String -> msg) -> Html msg
viewInput v toMsg =
  textarea [ onInput toMsg, style "width" "100%", style "height" "100%"] [text v]

informationalSection : List (String, String) -> Html msg
informationalSection items =
  div [] (List.map infoItem items)

infoItem (label, value) =
    div []
      [ text (label ++ ": ")
      , span (informational) [ text value ]
      ]

outputSection : String -> String -> Html msg
outputSection part1answer part2answer =
  div []
    [ text "Part 1: "
    , span (output) [ text "???" ]
    , text "Part 2: "
    , span (output) [ text "???" ]
    ]

-- FFFFFFFFUUUUU
sharedHeader day =
  div [ style "display" "flex"
      , style "justify-content" "space-around"
      , style "background" "#040"
      ]
    [ a [ href "/aoc/day0" ] [ text ("Day " ++ (day - 1 |> String.fromInt)) ]
    , div [] [ h3 (heading) [ text "Advent of Code" ], h4 (heading) [ text ("Day " ++ String.fromInt day) ] ]
    , a [ href "/aoc/day0" ] [ text ("Day " ++ (day + 1 |> String.fromInt)) ]
    ]

fetchInputForDay n msg =
  Http.get
  { url = "/aoc/input/DAY.txt" |> String.replace "DAY" (String.fromInt n)
  , expect = Http.expectString msg
  }

niceError httpErr =
  case httpErr of
    Http.BadUrl string -> "Bad URL: " ++ string
    Http.Timeout -> "Timeout"
    Http.NetworkError -> "Network error"
    Http.BadStatus int -> "HTTP " ++ String.fromInt int
    Http.BadBody string -> "Bad Body: " ++ string

