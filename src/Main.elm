port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Foo as F exposing (Model, Msg, encode, decoder)

import Json.Decode as D
import Json.Encode as E

type alias Model =
  { children : List F.Model
  }

type Msg
  = RemoveFromLocalStorage
  | AddChild
  | ChildAt Int F.Msg
  | RemoveAt Int

-- PORT
port store : E.Value -> Cmd msg
port remove : () -> Cmd msg

main : Program E.Value Model Msg
main =
  Browser.element
    {
      init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

init : E.Value -> ( Model, Cmd Msg )
init flags = 
  (
    case (D.decodeValue decoder flags) of
      Ok model -> model
      Err _ -> (Model [])
  , Cmd.none
  )

updateChild : Int -> F.Msg -> Int -> F.Model -> ( F.Model, Cmd Msg )
updateChild position childMsg index child =
  if index == position then
    case (F.update childMsg child) of
      ( newChild, childCmd ) ->
        ( newChild, (Cmd.map (ChildAt position) childCmd) )
  else
    ( child, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    RemoveFromLocalStorage ->
      ( model
      , (remove ())
      )
    AddChild ->
      let
        newModel = { model | children = (model.children ++ [ (F.new) ] ) }
      in
      ( newModel
      , (store (encode newModel))
      )
    RemoveAt position ->
      let
        newChildren = (List.take position model.children)
          ++ (List.drop (position + 1) model.children)
        newModel = { model | children = newChildren }
      in
      ( newModel
      , (store (encode newModel))
      )
    ChildAt position childMsg ->
      let
        newResults = (List.indexedMap (updateChild position childMsg) model.children)
        newChildren = (List.map (\(child, _) -> child) newResults)
        newCommands = (List.map (\(_, cmd) -> cmd) newResults)
        newModel = { model | children = newChildren }
      in
      ( newModel
      , (Cmd.batch (newCommands ++ [ (store (encode newModel)) ]))
      )

viewChild : Int -> F.Model -> Html Msg
viewChild index child =
  li
    [ style "margin-top" "1em"
    ]
    [ button
        [ onClick (RemoveAt index)
        ]
        [ text ("remove at " ++ (String.fromInt index))
        ]
    , (Html.map (ChildAt index) (F.view child))
    ]
            
view : Model -> Html Msg
view model =
  div
    []
    [ h1
        []
        [ text "Main"
        ]
    , div
        []
        [ button
            [ type_ "button"
            , onClick RemoveFromLocalStorage
            ]
            [ text "remove from local storage"
            ]
        ]
    , div
        []
        [ button
            [ onClick AddChild
            ]
            [ text "Add a child"
            ]
        ]
    , ol
        []
        (List.indexedMap viewChild model.children)
    , hr [] []
    ]

encode : Model -> E.Value
encode model =
  (E.object
    [ ("children", (E.list F.encode model.children))
    ]
  )

decoder : D.Decoder Model
decoder =
  (D.map
    Model
    (D.field "children" (D.list F.decoder))
  )
