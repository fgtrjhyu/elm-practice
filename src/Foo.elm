module Foo exposing (Model, Msg, new, update, view, encode, decoder)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Json.Decode as D
import Json.Encode as E

import Foo.Bar as B exposing (Model, Msg, encode, decoder, setPrefix)
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
        ( newChild
        , (Cmd.map (ChildAt position) childCmd)
        )
  else
    ( child
    , Cmd.none
    )

updatePrefixAt : String -> Int -> B.Model -> Cmd Msg
updatePrefixAt newText position _ =
  Cmd.map (ChildAt position) (B.setPrefix newText) 

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Add ->
      ( { model | value = model.value + 1 }
      , Cmd.none
      )
    SetText newText ->
      ( { model | text = newText }
      , (Cmd.batch (List.indexedMap (updatePrefixAt newText) model.children))
      )
    SetFlag newFlag ->
      ( { model | flag = newFlag }
      , Cmd.none
      )
    AddChild ->
      let
        prefix =
          if (String.isEmpty model.text) then
            B.new.prefix
          else
            model.text
        newChild = B.new
        newChildren = (model.children ++ [ { newChild | prefix = prefix } ])
        newModel = { model | children = newChildren }
      in
      ( newModel
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
      ( { model | children = newChildren }
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
        [ text "Foo" ]
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

encode : Model -> E.Value
encode model =
  E.object
    [ ("value", (E.int model.value))
    , ("text", (E.string model.text))
    , ("flag", (E.bool model.flag))
    , ("children", (E.list B.encode model.children))
    ]

decoder : D.Decoder Model
decoder =
  (D.map4
    Model
    (D.field "value" D.int)
    (D.field "text" D.string)
    (D.field "flag" D.bool)
    (D.field "children" (D.list B.decoder))
  )
