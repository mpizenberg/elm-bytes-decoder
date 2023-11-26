module Bytes.Decode.Branchable exposing (..)

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
import Bytes.Decode as D exposing (Step)


{-| A parser which tracks a certain type of context, a certain type of error and
produces a certain type of value.
-}
type Decoder value
    = Decoder (State -> D.Decoder ( State, value ))


type alias State =
    { input : Bytes
    , offset : Int
    }


succeed : value -> Decoder value
succeed val =
    fromDecoder (D.succeed val) 0


fail : Decoder value
fail =
    fromDecoder D.fail 0


run : Decoder value -> Bytes -> Maybe value
run parser input =
    runKeepState parser input
        -- |> Debug.log "(state, value)"
        |> Maybe.map (\( _, value ) -> value)


runKeepState : Decoder value -> Bytes -> Maybe ( State, value )
runKeepState (Decoder parser) input =
    let
        decoder =
            parser { input = input, offset = 0 }
    in
    D.decode decoder input


map : (a -> b) -> Decoder a -> Decoder b
map f (Decoder parser) =
    Decoder <|
        \state ->
            parser state
                |> D.map (Tuple.mapSecond f)


fromDecoder : D.Decoder v -> Int -> Decoder v
fromDecoder decoder byteLength =
    Decoder <|
        \state ->
            D.map
                (\v -> ( { input = state.input, offset = state.offset + byteLength }, v ))
                decoder


andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen thenB (Decoder parserA) =
    Decoder <|
        \state ->
            parserA state
                |> D.andThen
                    (\( newState, a ) ->
                        let
                            (Decoder parserB) =
                                thenB a
                        in
                        parserB newState
                    )


map2 : (x -> y -> z) -> Decoder x -> Decoder y -> Decoder z
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


oneOf : List (Decoder value) -> Decoder value
oneOf options =
    Decoder <|
        \state ->
            oneOfHelper (dropBytes state.offset state.input) options state


oneOfHelper : Bytes -> List (Decoder value) -> State -> D.Decoder ( State, value )
oneOfHelper offsetInput options state =
    case options of
        [] ->
            D.fail

        parser :: otherParsers ->
            case runKeepState parser offsetInput of
                Just ( newState, value ) ->
                    D.bytes newState.offset
                        |> D.map (\_ -> ( { input = state.input, offset = state.offset + newState.offset }, value ))

                Nothing ->
                    oneOfHelper offsetInput otherParsers state


dropBytes : Int -> Bytes -> Bytes
dropBytes offset bs =
    let
        width =
            Bytes.width bs
    in
    D.map2 (\_ x -> x) (D.bytes offset) (D.bytes <| width - offset)
        |> (\d -> D.decode d bs)
        |> Maybe.withDefault bs


loop : state -> (state -> Decoder (Step state a)) -> Decoder a
loop initialState callback =
    Decoder <|
        \initialParserState ->
            let
                makeParserStep : State -> Step state a -> Step ( state, State ) ( State, a )
                makeParserStep parserState step =
                    case step of
                        D.Loop state ->
                            D.Loop ( state, parserState )

                        D.Done a ->
                            D.Done ( parserState, a )

                loopStep : ( state, State ) -> D.Decoder (Step ( state, State ) ( State, a ))
                loopStep ( state, parserState ) =
                    let
                        (Decoder parser) =
                            callback state
                    in
                    parser parserState
                        -- Decoder (State, Step state a)
                        |> D.map (\( newParserState, step ) -> makeParserStep newParserState step)
            in
            D.loop ( initialState, initialParserState ) loopStep


repeat : Int -> Decoder value -> Decoder (List value)
repeat nTimes p =
    loop ( nTimes, [] ) (repeatHelp p)


repeatHelp :
    Decoder value
    -> ( Int, List value )
    -> Decoder (Step ( Int, List value ) (List value))
repeatHelp p ( cnt, acc ) =
    if cnt <= 0 then
        succeed (D.Done (List.reverse acc))

    else
        map (\v -> D.Loop ( cnt - 1, v :: acc )) p



-- Basics


{-| Parse one byte into an integer from 0 to 255.
-}
unsignedInt8 : Decoder Int
unsignedInt8 =
    fromDecoder D.unsignedInt8 1


{-| Parse two bytes into an integer from 0 to 65535.
-}
unsignedInt16 : Bytes.Endianness -> Decoder Int
unsignedInt16 bo =
    fromDecoder (D.unsignedInt16 bo) 2


unsignedInt32 : Bytes.Endianness -> Decoder Int
unsignedInt32 bo =
    fromDecoder (D.unsignedInt32 bo) 4


{-| Parse 4 bytes into a Float.
-}
float32 : Bytes.Endianness -> Decoder Float
float32 bo =
    fromDecoder (D.float32 bo) 4


float64 : Bytes.Endianness -> Decoder Float
float64 bo =
    fromDecoder (D.float64 bo) 8


{-| Parse `count` bytes as `Bytes`.
-}
bytes : Int -> Decoder Bytes
bytes count =
    fromDecoder (D.bytes count) count


string : Int -> Decoder String
string byteCount =
    fromDecoder (D.string byteCount) byteCount
