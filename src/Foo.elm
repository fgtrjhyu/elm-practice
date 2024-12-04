module Foo exposing (Model, Msg, new, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Json.Decode as D

import Foo.Bar as B exposing (Model, Msg, toSetPrefix)
import Task exposing (perform)

type alias Model =
  { value : Int
  , text : String
  , flag : Bool
  , children : List B.Model
  }

type Msg
  = Add
  | SetText String
  | SetFlag Bool
  | AddChild
  | RemoveAt Int
  | ChildAt Int B.Msg

new : Model
new =
  (Model 0 "" False [])

updateChild : Int -> B.Msg -> Int -> B.Model -> ( B.Model, Cmd Msg )
updateChild position childMsg index child =
  if index == position then
    case (B.update childMsg child) of
      (newChild, childCmd) ->
        (newChild, (Cmd.map (ChildAt position) childCmd))
  else
    ( child, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Add ->
      ( { model | value = model.value + 1 }
      , Cmd.none
      )
    SetText newText ->
      ( { model | text = newText }
      , Cmd.none
      )
    SetFlag newFlag ->
      ( { model | flag = newFlag }
      , Cmd.none
      )
    AddChild ->
      ( { model | children = model.children ++ [ B.new ] }
      , Cmd.none
      )
    RemoveAt position ->
      let
        newChildren = (List.take position model.children)
          ++ (List.drop (position + 1) model.children)
      in
      ( { model | children = newChildren }
      , Cmd.none
      )
    ChildAt position childMsg ->
      let
        newResults = (List.indexedMap (updateChild position childMsg) model.children)
        newChildren = (List.map (\(child, _) -> child) newResults)
        newCommands = (List.map (\(_, cmd) -> cmd) newResults)
      in
      ({ model | children = newChildren }
      , (Cmd.batch newCommands)
      )

viewChild : Int -> B.Model -> Html Msg
viewChild index child =
  li
    [ style "margin-top" "1em"
    ]
    [ button
        [ onClick (RemoveAt index)
        ]
        [ text ("remove at " ++ (String.fromInt index))
        ]
    , (Html.map (ChildAt index) (B.view child))
    ]

view : Model -> Html Msg
view model =
  div
    []
    [ h2
        []
        [ text "Model" ]
    , div
        []
        [ input
          [ type_ "text"
          , value model.text
          , onInput SetText
          ]
          []
        , span
            []
            [ text model.text
            ]
        , button
            [ onClick Add
            ]
            [ text "increase"
            ]
        , span
            []
            [ text (String.fromInt model.value)
            ]
        , input
            [ type_ "checkbox"
            , checked model.flag
            , on "change" (D.map SetFlag (D.at [ "target", "checked" ] D.bool))
            ]
            []
        , span
            []
            [ text (if model.flag then "Up" else "Down")
            ]
        ]
    , div
        [ style "margin-top" "1em"
        ]
        [ button
            [ onClick AddChild
            ]
            [ text "Add a child"
            ]
        ]
    , ol
        []
        (List.indexedMap viewChild model.children)
    ]
