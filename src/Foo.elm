module Foo exposing (Model, Msg, init, subscriptions, update, view, encode, decoder)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Json.Decode as D
import Json.Encode as E

import Foo.Bar as B exposing (Model, Msg, subscriptions, encode, decoder, setPrefix)
import Task
import Time

import TimeZoneOffset

type alias Model =
  { value : Int
  , text : String
  , flag : Bool
  , offset : TimeZoneOffset.TimeZoneOffset
  , time : Time.Posix
  , children : List B.Model
  }

type Msg
  = Add
  | SetText String
  | SetFlag Bool
  | SetTimeZoneOffset String
  | AddChild
  | RemoveAt Int
  | ChildAt Int B.Msg
  | Tick Time.Posix

init : (Model, Cmd Msg)
init =
  let
    offset = TimeZoneOffset.Ok(0)
  in
  ( (Model 0 "" False offset (Time.millisToPosix 0) [])
  , Cmd.none
  )

subscriptionsChildAt: Int -> B.Model -> Sub Msg
subscriptionsChildAt position model =
  let
    subMsg = (B.subscriptions model)
  in
  Sub.map (ChildAt position) subMsg

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    tickMsg = Time.every 1000 Tick
    subMessages = List.indexedMap subscriptionsChildAt model.children
  in
  Sub.batch (tickMsg::subMessages)

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
    SetTimeZoneOffset newString ->
      let
        newTimeZoneOffset = TimeZoneOffset.parse(newString)
      in
      ( { model | offset = newTimeZoneOffset }
      , Cmd.none
      )
    Tick newTime ->
      ( { model | time = newTime }
      , Cmd.none
      )
    AddChild ->
      let
        position = List.length model.children
        ( subModel, subMsg ) =
          if (String.isEmpty model.text) then
            (B.init "Hello ")
          else
            (B.init model.text)
        newChildren = ( model.children ++ [ subModel ] )
        newModel = { model | children = newChildren }
      in
      ( newModel
      , Cmd.map (ChildAt position) subMsg
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

viewChild : String -> Int -> B.Model -> Html Msg
viewChild viewpath index child =
  li
    [ style "margin-top" "1em"
    ]
    [ button
        [ onClick (RemoveAt index)
        ]
        [ text ("remove at " ++ (String.fromInt index))
        ]
    , (Html.map (ChildAt index) (B.view viewpath child))
    ]

timezoneOffsetText: Int -> String
timezoneOffsetText int =
  if int > 0 then
     ("+" ++ (String.fromInt int))
  else if int < 0 then
     (String.fromInt int)
  else 
     ("±" ++ (String.fromInt int))

viewTimeZoneOffsetDatalistOption: Int -> Html Msg
viewTimeZoneOffsetDatalistOption int =
  option
    [ value (String.fromInt int)
    ]
    [ text (timezoneOffsetText int)
    ]

viewTimeZoneOffsetDatalist: String -> Html Msg
viewTimeZoneOffsetDatalist listId =
  datalist
    [ id listId
    ]
    (List.map viewTimeZoneOffsetDatalistOption (List.range -12 13))

viewTimeZoneOffsetInput: String -> String -> Html Msg
viewTimeZoneOffsetInput model listId =
    div
      []
      [ label
          []
          [ text "Time Zone Offset : "
          , input
            [ type_ "text"
            , onInput SetTimeZoneOffset
            , list listId
            , value model
            , name "timeZoneOffset"
            ]
            [
            ]
          , viewTimeZoneOffsetDatalist listId
          ]
      ]

viewTime : TimeZoneOffset.TimeZoneOffset -> Time.Posix -> String -> Html Msg
viewTime offset time listId =
  case offset of
    TimeZoneOffset.Invalid string ->
      node "foo-bar" 
        [ attribute "error" "invalid"
        ]
        [ div
            [ attribute "slot" "error-message" ]
            [ text "Invalid Input Text"
            ]
        , div
            [ attribute "slot" "input" ]
            [ viewTimeZoneOffsetInput string listId
            ]
        ]
    TimeZoneOffset.TooSmall int ->
      node "foo-bar" 
        [ attribute "error" "too-small"
        ]
        [ div
            [ attribute "slot" "error-message" ]
            [ text ((String.fromInt int) ++ " is too small.")
            ]
        , div
            [ attribute "slot" "input" ]
            [ viewTimeZoneOffsetInput (timezoneOffsetText int) listId
            ]
        ]
    TimeZoneOffset.TooLarge int ->
      node "foo-bar" 
        [ attribute "error" "too-large"
        ]
        [ div
            [ attribute "slot" "error-message" ]
            [ text ((String.fromInt int) ++ " is too large.")
            ]
        , div
            [ attribute "slot" "input" ]
            [ viewTimeZoneOffsetInput (timezoneOffsetText int) listId
            ]
        ]
    TimeZoneOffset.Ok offsetHour ->
      let
        zone = (Time.customZone (offsetHour * 60) [])
        get fn =
          String.right 2 ("0" ++ (String.fromInt (fn zone time)))
        viewValue =
          [ Time.toHour, Time.toMinute, Time.toSecond ]
            |> List.map get
            |> List.intersperse ":"
            |> List.foldr (\a b -> a ++ b) ""
      in
      node "foo-bar" 
        [
        ]
        [ div
            [ attribute "slot" "time" ]
            [ text viewValue
            ]
        , div
            [ attribute "slot" "input" ]
            [ viewTimeZoneOffsetInput (timezoneOffsetText offsetHour) listId
            ]
        ]

viewGreetingMessage : String -> Model -> String -> Html Msg
viewGreetingMessage viewpath model listId =
  let
    messageOption message = option [ value message ] []
    maybeZone = TimeZoneOffset.toZone model.offset
    messages =
      case maybeZone of
        Nothing ->
          [ "Hello" ]
        Just zone ->
          let
            hour = Time.toHour zone model.time
          in
          if hour >= 0 && hour < 4  then
            [ "Good evening", "Good morning", "Good afternoon", "Hello" ]
          else if hour >= 4 && hour < 11 then
            [ "Good mortninig", "Good afternoon", "Good evening", "Hello" ]
          else if hour >= 11 && hour < 14 then
            [ "Good afternonn", "Good evening", "Hello", "Good morning" ]
          else
            [ "Good evening", "Hello", "Good morning", "Good afternonn" ]
  in
  div
    []
    [ div
        []
        [ label
            []
            [ text "Greeting Message : "
            , input
              [ type_ "text"
              , value model.text
              , onInput SetText
              , list listId
              ]
              []
            ]
        , datalist
            [ id listId
            ]
            (List.map messageOption messages)
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
        (List.indexedMap (viewChild viewpath) model.children)
    ]

view : String -> Model -> Html Msg
view viewpath model =
  div
    []
    [ h2
        []
        [ text "Foo" ]
    , div
        []
        [ button
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
    , hr [][]
    , viewTime model.offset model.time (viewpath ++ ".timezone-offset-datalist")
    , hr [][]
    , viewGreetingMessage viewpath model (viewpath ++ ".greeting-message-datalist")
    ]

encode : Model -> E.Value
encode model =
  E.object
    [ ("value", (E.int model.value))
    , ("text", (E.string model.text))
    , ("flag", (E.bool model.flag))
    , ("offset", (TimeZoneOffset.encode model.offset))
    , ("zone", E.null)
    , ("time", E.null)
    , ("children", (E.list B.encode model.children))
    ]

decoder : D.Decoder Model
decoder =
  (D.map6
    Model
    (D.field "value" D.int)
    (D.field "text" D.string)
    (D.field "flag" D.bool)
    (D.field "offset" TimeZoneOffset.decoder)
    (D.field "time" (D.succeed (Time.millisToPosix 0)))
    (D.field "children" (D.list B.decoder))
  )
