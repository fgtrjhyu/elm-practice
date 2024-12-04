module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Foo as F exposing (Model, Msg)

type alias Model =
  { children : List F.Model
  }

type Msg
  = AddChild
  | ChildAt Int F.Msg
  | RemoveAt Int

main : Program () Model Msg
main =
  Browser.element
    {
      init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

init : () -> ( Model, Cmd Msg )
init _ = 
  (
    (Model [])
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
    AddChild -> ( { model | children = (model.children ++ [ (F.new) ] ) }, Cmd.none )
    RemoveAt position ->
      let
        newChildren = (List.take position model.children)
          ++ (List.drop (position + 1) model.children)
      in
      ({ model | children = newChildren }
      , Cmd.none
      )
    ChildAt position childMsg ->
      let
        newResults = (List.indexedMap (updateChild position childMsg) model.children)
        newChildren = (List.map (\(child, _) -> child) newResults)
        newCommands = (List.map (\(_, cmd) -> cmd) newResults)
        _ = (Debug.log "a" newCommands)
      in
      ({ model | children = newChildren }
      , (Cmd.batch newCommands)
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
