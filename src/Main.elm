port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Url
import Url.Parser as P exposing (Parser, (</>), (<?>))
import Url.Parser.Query as PQ

import Foo as F exposing (Model, Msg, encode, decoder)

import Json.Decode as D
import Json.Encode as E

type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , children : List F.Model
  , lang : String
  , year : Int
  , month : Int
  }

type Route
  = NotFound
  | Home
  | Profile (Maybe String)

routeParser : P.Parser (Route -> a) a
routeParser =
  P.oneOf
    [ P.map Home P.top
    , P.map Home (P.s "home")
    , P.map Home (P.s "src" </> P.s "Main.elm")
    , P.map Profile (P.s "profile" </> P.fragment identity)
    ]

toRoute : Url.Url -> Route
toRoute url =
  Maybe.withDefault NotFound (P.parse routeParser url)

type Msg
  = RemoveFromLocalStorage
  | AddChild
  | ChildAt Int F.Msg
  | RemoveAt Int
  | LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url

-- PORT
port store : E.Value -> Cmd msg
port remove : () -> Cmd msg


main : Program E.Value Model Msg
main =
  Browser.application
    { init = init
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

init : E.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key = 
  (case (D.decodeValue (decoder (Model key url)) flags) of
      Ok model -> model
      Err _ -> (Model key url [] "jp" 2025 12)
  , Cmd.none)

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
    LinkClicked urlRequest ->
      let
        _ = (Debug.log "LinkClicked" urlRequest)
        cmd =
          case urlRequest of
            Browser.Internal url ->
              Nav.pushUrl model.key (Url.toString url)
            Browser.External href ->
              Nav.load href
      in
      ( model
      , cmd
      )
    UrlChanged url ->
      let
        _ = (Debug.log "UrlChanged" url)
        tmpModel = { model | url = url }
        newModel = case (toRoute url) of
          NotFound -> tmpModel
          Home -> tmpModel
          Profile fragment -> tmpModel
      in
      ( newModel
      , Cmd.none
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

viewLink : String -> Html Msg
viewLink path =
  li
    []
    [
      a
        [ href path ]
        [ text path ]
    ]

viewLinks : Model -> Html Msg
viewLinks model =
  div
    []
    [ text "The Current URL is "
    , b [] [ text (Url.toString model.url) ]
    , ul
        []
        [ viewLink "/home" 
        , viewLink "/profile" 
        , viewLink "/notfound" 
        , viewLink "http://www.google.com"
        ]
    ]

viewProfile : Model -> Html Msg
viewProfile model =
  div
    []
    [ (viewLinks model)
    , h1
        []
        [ text "Profile"
        ]
    ]

viewHome : Model -> Html Msg
viewHome model =
  div
    []
    [ (viewLinks model)
    , h1
        []
        [ text "Main"
        ]
    , node "intl-date"
        [ attribute "lang" model.lang
        , attribute "year" (String.fromInt model.year)
        , attribute "month" (String.fromInt model.month)
        ]
        []
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

viewNotFound : Model -> Html Msg
viewNotFound model =
  div
    []
    [
      h1
        []
        [ text "Not Found"
        ]
    ]

viewUrl : Model -> Html Msg
viewUrl model =
  case (toRoute model.url) of
    Home -> viewHome model
    Profile fragment -> viewProfile model
    NotFound -> viewNotFound model

title : Model -> String
title model =
  "Number of children = " ++ (String.fromInt (List.length model.children))

view : Model -> Browser.Document Msg
view model =
  { title = (title model)
  , body = [ (viewUrl model) ]
  }

encode : Model -> E.Value
encode model =
  (E.object
    [ ("children", (E.list F.encode model.children))
    , ("lang", (E.string model.lang))
    , ("year", (E.int model.year))
    , ("month", (E.int model.month))
    ]
  )

decoder : (List F.Model -> String -> Int -> Int -> Model) -> D.Decoder Model
decoder model =
  (D.map4
    model
    (D.field "children" (D.list F.decoder))
    (D.field "lang" (D.string))
    (D.field "year" (D.int))
    (D.field "month" (D.int))
  )
