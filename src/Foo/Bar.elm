module Foo.Bar exposing (Model, Msg, update, view, new, encode, decoder, setPrefix)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Json.Decode as D
import Json.Encode as E

import Task

type Msg = OptionChanged String
  | SetPrefix String

type alias Option =
  { value : String
  , description : String
  }

type alias Model =
  { prefix : String
  , project : String
  }

computingMascots : List (String, String)
computingMascots =
  [ ("Rust", "Ferris")
  , ("Java", "Duke")
  , ("Plan9", "Grlenda")
  , ("Go", "Gopher")
  ]

new : Model
new =
  (Model "Hello, " "Rust")

setPrefix : String -> Cmd Msg
setPrefix newPrefix =
  Task.perform SetPrefix (Task.succeed newPrefix)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    OptionChanged newValue ->
      ( { model | project = newValue  }
      , Cmd.none
      )
    SetPrefix newValue ->
      ( { model | prefix = newValue  }
      , Cmd.none
      )

viewOption : String -> (String, String) -> Html Msg
viewOption choosed (key, display) =
  option
    [ value key
    , selected (choosed == key)
    ]
    [ text key
    ]

view : Model -> Html Msg
view model =
  let
    (project, mascot) =
      case (List.head (List.filter (\(key, _) -> key == model.project) computingMascots)) of
      Just item -> item
      Nothing -> ("", "")
  in
  div
    []
    [ h3
        []
        [ text "Foo.Model"
        ]
    , select
        [ value model.project
        , on "change" (D.map OptionChanged (D.at [ "target", "value" ] D.string))
        ]
        (List.map (viewOption model.project) computingMascots)
    , span
        []
        [ text (model.prefix ++ " " ++ mascot ++ "!") 
        ]
    ]

encode : Model -> E.Value
encode model =
  E.object
    [ ("prefix", (E.string model.prefix))
    , ("project", (E.string model.project))
    ]

decoder : D.Decoder Model
decoder =
  (D.map2
    Model
    (D.field "prefix" D.string)
    (D.field "project" D.string)
  )

