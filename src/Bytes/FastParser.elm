module Bytes.FastParser exposing (..)

{-| Parse `Bytes` with custom error reporting and context tracking.


# Running parsers

@docs Parser, run, Error


# Static parsers

@docs succeed, fail, inContext


# Basic parsers


## Integers

@docs unsignedInt8, unsignedInt16, unsignedInt32, signedInt8, signedInt16, signedInt32


## Floats

@docs float32, float64


## Strings

@docs string


## Bytes

@docs bytes


# Transforming values

@docs map, map2, map3, map4, map5


# Combininig parsers

@docs keep, ignore, skip


# Fancy parsers

@docs andThen, oneOf, repeat, Step, loop


# Random access

@docs Position, position, startOfInput, randomAccess

-}

import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder, Step)


{-| A parser which tracks a certain type of context, a certain type of error and
produces a certain type of value.
-}
type alias Parser value =
    State -> Decoder ( State, value )


type alias State =
    { input : Bytes
    , offset : Int
    }


succeed : value -> Parser value
succeed val state =
    fromDecoder (Decode.succeed val) 0 state


fail : Parser value
fail state =
    fromDecoder Decode.fail 0 state


forEver : a -> b
forEver a =
    forEver a


forEverDecoder : Decoder a
forEverDecoder =
    Decode.map forEver (Decode.succeed ())


run : Parser value -> Bytes -> Maybe value
run parser input =
    let
        decoder =
            parser { input = input, offset = 0 }
    in
    Decode.decode decoder input
        |> Maybe.map Tuple.second


map : (a -> b) -> Parser a -> Parser b
map f parser state =
    parser state
        |> Decode.map (Tuple.mapSecond f)


fromDecoder : Decoder v -> Int -> Parser v
fromDecoder decoder byteLength state =
    Decode.map
        (\v -> ( { input = state.input, offset = state.offset + byteLength }, v ))
        decoder


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen thenB parser state =
    parser state
        |> Decode.andThen
            (\( newState, a ) ->
                thenB a newState
            )


map2 : (x -> y -> z) -> Parser x -> Parser y -> Parser z
map2 f parserX parserY =
    parserX |> andThen (\x -> map (\y -> f x y) parserY)



-- keep :
--     Parser error a
--     -> Parser error (a -> b)
--     -> Parser error b
-- keep val fun =
--     map2 (<|) fun val
-- ignore :
--     Parser error ignore
--     -> Parser error keep
--     -> Parser error keep
-- ignore skipper keeper =
--     map2 always keeper skipper
-- skip : Int -> Parser error value -> Parser error value
-- skip nBytes =
--     ignore (bytes nBytes)
-- oneOf : List (Parser error value) -> Parser error value
-- oneOf options =
--     Parser (oneOfHelp options [])
-- oneOfHelp :
--     List (Parser error value)
--     -> List (Error error)
--     -> State
--     -> ParseResult error value
-- oneOfHelp options errors state =
--     case options of
--         [] ->
--             Bad (BadOneOf { at = state.offset } (List.reverse errors))
--         (Parser f) :: xs ->
--             case f state of
--                 Good v s ->
--                     Good v s
--                 Bad e ->
--                     oneOfHelp xs (e :: errors) state


loop : state -> (state -> Parser (Step state a)) -> Parser a
loop initialState callback initialParserState =
    let
        parsifiedCallback : State -> state -> Decoder (Step state ( State, a ))
        parsifiedCallback parserState state =
            let
                stepDecoder : Decoder ( State, Step state a )
                stepDecoder =
                    callback state parserState

                moveParserStateIntoStep : ( State, Step state a ) -> Step state ( State, a )
                moveParserStateIntoStep ( ps, step ) =
                    stepMap (\a -> ( ps, a )) step
            in
            Decode.map moveParserStateIntoStep stepDecoder
    in
    Decode.loop initialState (parsifiedCallback initialParserState)


stepMap : (a -> b) -> Step state a -> Step state b
stepMap f step =
    case step of
        Decode.Loop state ->
            Decode.Loop state

        Decode.Done a ->
            Decode.Done (f a)



-- repeat : Parser error value -> Int -> Parser error (List value)
-- repeat p nTimes =
--     loop (repeatHelp p) ( nTimes, [] )
-- repeatHelp :
--     Parser error value
--     -> ( Int, List value )
--     -> Parser error (Step ( Int, List value ) (List value))
-- repeatHelp p ( cnt, acc ) =
--     if cnt <= 0 then
--         succeed (Done (List.reverse acc))
--     else
--         map (\v -> Loop ( cnt - 1, v :: acc )) p
-- Basics


{-| Parse one byte into an integer from 0 to 255.
-}
unsignedInt8 : Parser Int
unsignedInt8 =
    fromDecoder Decode.unsignedInt8 1


{-| Parse two bytes into an integer from 0 to 65535.
-}
unsignedInt16 : Bytes.Endianness -> Parser Int
unsignedInt16 bo =
    fromDecoder (Decode.unsignedInt16 bo) 2


{-| Parse 4 bytes into a Float.
-}
float32 : Bytes.Endianness -> Parser Float
float32 bo =
    fromDecoder (Decode.float32 bo) 4


{-| Parse `count` bytes as `Bytes`.
-}
bytes : Int -> Parser Bytes
bytes count =
    fromDecoder (Decode.bytes count) count


string : Int -> Parser String
string byteCount =
    fromDecoder (Decode.string byteCount) byteCount
