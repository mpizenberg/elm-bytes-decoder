module Bytes.FastParser exposing
    ( Parser, run, Error(..)
    , succeed, fail
    , unsignedInt8, unsignedInt16
    , float32
    , string
    , bytes
    , map, map2
    , keep, ignore, skip
    , andThen, oneOf, repeat, Step(..), loop
    )

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
import Bytes.Decode as Decode exposing (Decoder)


{-| A parser which tracks a certain type of context, a certain type of error and
produces a certain type of value.
-}
type Parser error value
    = Parser (State -> ParseResult error value)


type ParseResult error value
    = Good value State
    | Bad (Error error)


type Error error
    = OutOfBounds { at : Int, bytes : Int }
    | Custom { at : Int } error
    | BadOneOf { at : Int } (List (Error error))


type alias State =
    { offset : Int
    , input : Bytes
    }


fromDecoder : Decoder v -> Int -> Parser error v
fromDecoder dec byteLength =
    Parser <|
        \state ->
            let
                combine =
                    Decode.map2 (\_ x -> x) (Decode.bytes state.offset) dec
            in
            case Decode.decode combine state.input of
                Just res ->
                    Good res { offset = state.offset + byteLength, input = state.input }

                Nothing ->
                    Bad (OutOfBounds { at = state.offset, bytes = byteLength })


succeed : value -> Parser error value
succeed val =
    Parser (Good val)


fail : error -> Parser error value
fail e =
    Parser <| \state -> Bad (Custom { at = state.offset } e)


run :
    Parser error value
    -> Bytes
    -> Result (Error error) value
run (Parser f) input =
    let
        initialState : State
        initialState =
            { offset = 0
            , input = input
            }
    in
    case f initialState of
        Good v _ ->
            Ok v

        Bad e ->
            Err e


map :
    (a -> b)
    -> Parser error a
    -> Parser error b
map t (Parser f) =
    Parser <|
        \state ->
            case f state of
                Good v s ->
                    Good (t v) s

                Bad e ->
                    Bad e


andThen :
    (a -> Parser error b)
    -> Parser error a
    -> Parser error b
andThen toParserB (Parser f) =
    Parser <|
        \state ->
            case f state of
                Good v s ->
                    let
                        (Parser p) =
                            toParserB v
                    in
                    p s

                Bad e ->
                    Bad e


map2 :
    (x -> y -> z)
    -> Parser error x
    -> Parser error y
    -> Parser error z
map2 f parserX parserY =
    parserX |> andThen (\x -> parserY |> andThen (\y -> succeed (f x y)))


keep :
    Parser error a
    -> Parser error (a -> b)
    -> Parser error b
keep val fun =
    map2 (<|) fun val


ignore :
    Parser error ignore
    -> Parser error keep
    -> Parser error keep
ignore skipper keeper =
    map2 always keeper skipper


skip : Int -> Parser error value -> Parser error value
skip nBytes =
    ignore (bytes nBytes)


oneOf : List (Parser error value) -> Parser error value
oneOf options =
    Parser (oneOfHelp options [])


oneOfHelp :
    List (Parser error value)
    -> List (Error error)
    -> State
    -> ParseResult error value
oneOfHelp options errors state =
    case options of
        [] ->
            Bad (BadOneOf { at = state.offset } (List.reverse errors))

        (Parser f) :: xs ->
            case f state of
                Good v s ->
                    Good v s

                Bad e ->
                    oneOfHelp xs (e :: errors) state


type Step state a
    = Loop state
    | Done a


loop :
    (state -> Parser error (Step state a))
    -> state
    -> Parser error a
loop toNext initialState =
    Parser (loopHelp initialState toNext)


loopHelp :
    state
    -> (state -> Parser error (Step state a))
    -> State
    -> ParseResult error a
loopHelp loopState toNext state =
    let
        (Parser next) =
            toNext loopState
    in
    case next state of
        Good (Loop newLoopState) newState ->
            loopHelp newLoopState toNext newState

        Good (Done v) newState ->
            Good v newState

        Bad e ->
            Bad e


repeat : Parser error value -> Int -> Parser error (List value)
repeat p nTimes =
    loop (repeatHelp p) ( nTimes, [] )


repeatHelp :
    Parser error value
    -> ( Int, List value )
    -> Parser error (Step ( Int, List value ) (List value))
repeatHelp p ( cnt, acc ) =
    if cnt <= 0 then
        succeed (Done (List.reverse acc))

    else
        map (\v -> Loop ( cnt - 1, v :: acc )) p



-- Basics


{-| Parse one byte into an integer from 0 to 255.
-}
unsignedInt8 : Parser error Int
unsignedInt8 =
    fromDecoder Decode.unsignedInt8 1


{-| Parse two bytes into an integer from 0 to 65535.
-}
unsignedInt16 : Bytes.Endianness -> Parser error Int
unsignedInt16 bo =
    fromDecoder (Decode.unsignedInt16 bo) 2


{-| Parse 4 bytes into a Float.
-}
float32 : Bytes.Endianness -> Parser error Float
float32 bo =
    fromDecoder (Decode.float32 bo) 4


{-| Parse `count` bytes as `Bytes`.
-}
bytes : Int -> Parser error Bytes
bytes count =
    fromDecoder (Decode.bytes count) count


{-| Parse `count` bytes representing UTF-8 characters into a String.

Note that Elm strings use UTF-16. As a result, the `String.length` will not
always agree with the number of bytes that went into it!

    import Bytes.Encode as E
    import Bytes.Parser as P


    [ 0xF0, 0x9F, 0x91, 0x8D ]
        |> List.map E.unsignedInt8
        |> E.sequence
        |> E.encode
        |> P.run (P.string 4)
    --> Ok "👍"

-}
string : Int -> Parser error String
string byteCount =
    fromDecoder (Decode.string byteCount) byteCount
