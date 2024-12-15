module TimeZoneOffset exposing (TimeZoneOffset(..), parse, toZone, toString, encode, decoder)

import Time

import Json.Encode as E
import Json.Decode as D

type TimeZoneOffset
  = Invalid String
  | TooSmall Int
  | TooLarge Int
  | Ok Int

toZone : TimeZoneOffset -> Maybe Time.Zone
toZone offset =
  case offset of
    Ok hour -> Just (Time.customZone (hour * 60) [])
    _ -> Nothing

fromInt : Int -> TimeZoneOffset
fromInt int =
  if int < -12 then
    TooSmall int
  else if int > 13 then
    TooLarge int
  else
    Ok int

parse : String -> TimeZoneOffset
parse string =
  case (String.toInt string) of
    Nothing -> Invalid string
    Just int -> fromInt int

toString : TimeZoneOffset -> String
toString offset =
  case offset of
    Invalid string -> string
    TooSmall int -> (String.fromInt int)
    TooLarge int -> (String.fromInt int)
    Ok int -> (String.fromInt int)

encode : TimeZoneOffset -> E.Value
encode offset =
  case offset of
    Invalid string -> (E.string string)
    TooSmall int -> (E.int int)
    TooLarge int -> (E.int int)
    Ok int -> (E.int int)

decoder : D.Decoder TimeZoneOffset
decoder =
  D.oneOf
    [ D.map fromInt D.int
    , D.map Invalid D.string
    ]
    
    

