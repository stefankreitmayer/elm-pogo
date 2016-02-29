import Svg exposing (..)
import Svg.Attributes exposing (..)

main =
  svg [ version "1.1", x "0", y "0", viewBox "0 0 400 400" ]
    [
      definitions
      , background
      , hand
      , hair
      , mouth
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

hair =
  Svg.path [
    fill "#00AD00", d "M 363,194 C 370,270 195,159 191,323 89,113 352,74.8 363,194 Z"
    , Svg.Attributes.style "fill:url(#grad1)"
  ] []

hand =
  Svg.path [
    fill "#ffd5d5", d "M 149,61 C 149,23 129,31 127,61 126,89 126,106 125,142 101,124 71.5,137 60,142 57.7,89 57.6,62 46.8,62 34.8,64 40,106 40,159 40,195 54.6,230 96,230 137,230 151,212 151,177 151,124 150,124 149, 61 Z"
  ] []

mouth =
  Svg.path [
    fill "#ffd5d5", d "M 255,307 C 309,324 319,307 319,307 319,307 311,324 289,324 266,324 255,307 255,307 Z"
  ] []
