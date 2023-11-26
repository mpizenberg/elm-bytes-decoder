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
type Parser value
    = Parser (State -> Decoder ( State, value ))


type alias State =
    { input : Bytes
    , offset : Int
    }


succeed : value -> Parser value
succeed val =
    fromDecoder (Decode.succeed val) 0


fail : Parser value
fail =
    fromDecoder Decode.fail 0


forEver : a -> b
forEver a =
    forEver a


forEverDecoder : Decoder a
forEverDecoder =
    Decode.map forEver (Decode.succeed ())


run : Parser value -> Bytes -> Maybe value
run parser input =
    runKeepState parser input
        -- |> Debug.log "(state, value)"
        |> Maybe.map (\( _, value ) -> value)


runKeepState : Parser value -> Bytes -> Maybe ( State, value )
runKeepState (Parser parser) input =
    let
        decoder =
            parser { input = input, offset = 0 }
    in
    Decode.decode decoder input


map : (a -> b) -> Parser a -> Parser b
map f (Parser parser) =
    Parser <|
        \state ->
            parser state
                |> Decode.map (Tuple.mapSecond f)


fromDecoder : Decoder v -> Int -> Parser v
fromDecoder decoder byteLength =
    Parser <|
        \state ->
            Decode.map
                (\v -> ( { input = state.input, offset = state.offset + byteLength }, v ))
                decoder


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen thenB (Parser parserA) =
    Parser <|
        \state ->
            parserA state
                |> Decode.andThen
                    (\( newState, a ) ->
                        let
                            (Parser parserB) =
                                thenB a
                        in
                        parserB newState
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


oneOf : List (Parser value) -> Parser value
oneOf options =
    Parser <|
        \state ->
            oneOfHelper (dropBytes state.offset state.input) options state


oneOfHelper : Bytes -> List (Parser value) -> State -> Decoder ( State, value )
oneOfHelper offsetInput options state =
    case options of
        [] ->
            Decode.fail

        parser :: otherParsers ->
            case runKeepState parser offsetInput of
                Just ( newState, value ) ->
                    Decode.bytes newState.offset
                        |> Decode.map (\_ -> ( { input = state.input, offset = state.offset + newState.offset }, value ))

                Nothing ->
                    oneOfHelper offsetInput otherParsers state


dropBytes : Int -> Bytes -> Bytes
dropBytes offset bs =
    let
        width =
            Bytes.width bs
    in
    Decode.map2 (\_ x -> x) (Decode.bytes offset) (Decode.bytes <| width - offset)
        |> (\d -> Decode.decode d bs)
        |> Maybe.withDefault bs


loop : state -> (state -> Parser (Step state a)) -> Parser a
loop initialState callback =
    Parser <|
        \initialParserState ->
            let
                makeParserStep : State -> Step state a -> Step ( state, State ) ( State, a )
                makeParserStep parserState step =
                    case step of
                        Decode.Loop state ->
                            Decode.Loop ( state, parserState )

                        Decode.Done a ->
                            Decode.Done ( parserState, a )

                loopStep : ( state, State ) -> Decoder (Step ( state, State ) ( State, a ))
                loopStep ( state, parserState ) =
                    let
                        (Parser parser) =
                            callback state
                    in
                    parser parserState
                        -- Decoder (State, Step state a)
                        |> Decode.map (\( newParserState, step ) -> makeParserStep newParserState step)
            in
            Decode.loop ( initialState, initialParserState ) loopStep


repeat : Int -> Parser value -> Parser (List value)
repeat nTimes p =
    loop ( nTimes, [] ) (repeatHelp p)


repeatHelp :
    Parser value
    -> ( Int, List value )
    -> Parser (Step ( Int, List value ) (List value))
repeatHelp p ( cnt, acc ) =
    if cnt <= 0 then
        succeed (Decode.Done (List.reverse acc))

    else
        map (\v -> Decode.Loop ( cnt - 1, v :: acc )) p



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


unsignedInt32 : Bytes.Endianness -> Parser Int
unsignedInt32 bo =
    fromDecoder (Decode.unsignedInt32 bo) 4


{-| Parse 4 bytes into a Float.
-}
float32 : Bytes.Endianness -> Parser Float
float32 bo =
    fromDecoder (Decode.float32 bo) 4


float64 : Bytes.Endianness -> Parser Float
float64 bo =
    fromDecoder (Decode.float64 bo) 8


{-| Parse `count` bytes as `Bytes`.
-}
bytes : Int -> Parser Bytes
bytes count =
    fromDecoder (Decode.bytes count) count


string : Int -> Parser String
string byteCount =
    fromDecoder (Decode.string byteCount) byteCount
