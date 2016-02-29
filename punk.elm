import String exposing (concat)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (every)

main =
  Signal.map scene (every 25)

scene t =
  svg [ version "1.1", x "0", y "0", viewBox "0 0 400 400" ]
    [
      definitions
      , background
      , hair t
      , mouth t
      , hand t
    ]

definitions =
  defs [] [
    linearGradient [id "grad1", x1 "50%", y1 "0%", x2 "100%", y2 "0%"]
      [ stop [offset "0%", Svg.Attributes.style "stop-color:rgb(255,128,0)"] []
      , stop [offset "100%", Svg.Attributes.style "stop-color:rgb(200,39,0)"] []
    ]
  ]

background =
  rect [fill "none", fill "#111111", x "0", y "0", width "400", height "400"] []

hair t =
  let
    startEndX = (around t 363 3 1.0) |> toString
    startEndY = (around t 194 30 1.2) |> toString
    translateX = (around t 0 0 1.2) |> toString
    translateY = (around t 0 0 1.2) |> toString
    rotate = (around t 0 10 0.5) |> toString
  in
    Svg.path [
      fill "#00AD00"
      , d (String.join " "
        [ "M"
        , startEndX
        , startEndY
        , "C"
        , (around t 370 0 3) |> toString--
        , (around t 270 0 3) |> toString
        , (around t 195 50 1.2) |> toString--
        , (around t 159 40 1.3) |> toString
        , (around t 191 25 1.2) |> toString--tip
        , (around t (around t 323 20 0.7) 25 1.2) |> toString
        , (around t 89 50 1.2) |> toString--
        , (around t (around t 113 17 2) 15 1.6) |> toString
        , (around t 352 10 0.9) |> toString--
        , (around t 74.8 20 1.4) |> toString
        , startEndX
        , startEndY
        , "Z"
        ])
      , Svg.Attributes.style "fill:url(#grad1)"
      , String.concat ["translate(", translateX ,",", translateY, ") rotate(", rotate ,")"] |> transform
    ] []

around t center fluctuation frequency =
  center + (sin (t / 1000 * 6.28 * frequency)) * fluctuation

hand t =
  let
    scaleX = (around t 0.8 -0.2 1.2) |> toString
    scaleY = (around t 0.6 0.3 1.2) |> toString
    translateX = (around t 0 20 1.2) |> toString
    translateY = (around t 0 -100 1.2) |> toString
  in
    Svg.path [
      fill "#ffd5d5"
      , d "M 149,61 C 149,23 129,31 127,61 126,89 126,106 125,142 101,124 71.5,137 60,142 57.7,89 57.6,62 46.8,62 34.8,64 40,106 40,159 40,195 54.6,230 96,230 137,230 151,212 151,177 151,124 150,124 149, 61 Z"
      , String.concat ["translate(", translateX ,",", translateY, ") scale(", scaleX ,",", scaleY, ")"] |> transform
    ] []

mouth t =
  let
    startEndX = (around t 255 -4 0.2) |> toString
    startEndY = (around t 307 40 1.2) |> toString
    tipX = (around t 319 5 0.2) |> toString
    tipY = (around t 307 40 1.2) |> toString
    bottomY = (around t 324 40 1.2) |> toString
    rotate = (around t 0 10 0.5) |> toString
  in
    Svg.path [
      fill "#ffd5d5"
      , d (String.join " "
        [ "M"
        , startEndX
        , startEndY
        , "C"
        , (around t 309 0 3) |> toString--
        , bottomY
        , tipX
        , tipY
        , tipX
        , tipY
        , tipX
        , tipY
        , (around t 311 0 1.2) |> toString--
        , bottomY
        , (around t 289 10 0.13) |> toString--
        , bottomY
        , (around t 266 20 0.13) |> toString--
        , bottomY
        , startEndX
        , startEndY
        , startEndX
        , startEndY
        , "Z"
        ])
      , String.concat ["rotate(", rotate ,")"] |> transform
    ] []
