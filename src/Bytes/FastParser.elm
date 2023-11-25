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


{-| Describes errors that arise while parsing.

Custom errors happen through [`fail`](#fail), context tracking happens through
[`inContext`](#inContext).

-}
type Error error
    = OutOfBounds { at : Int, bytes : Int }
    | Custom { at : Int } error
    | BadOneOf { at : Int } (List (Error error))


type alias State =
    { offset : Int
    , input : Bytes
    }


succeed : value -> Parser error value
succeed val =
    Parser (Good val)


{-| A Parser that always fails with the given error.

    import Bytes.Encode as E
    import Bytes.Parser as P


    type Error = SomeFailure


    E.sequence []
        |> E.encode
        |> P.run (P.fail SomeFailure)
    --> Err (P.Custom { at = 0 } SomeFailure)

_Important note about using `fail` in `andThen`_:

The offset the `Custom` constructor of `Error` is tagged with, is the offset the
parser is at when `fail` is executed. When this happens inside and `andThen`, be
aware that something was already read in order for there to be and `andThen` in
the first place.

For example, consider this:

    E.unsignedInt8 1
        |> E.encode
        |> P.run (P.andThen (\_ -> P.fail "fail") P.unsignedInt8)
    --> Err (P.Custom { at = 1 } "fail")

We may have intended for the failure to be about the byte we just read, and
expect the offset to be "before" reading that byte. That's not quite what
`andThen` means, though! `andThen` means we parsed something successfully
already!

-}
fail : error -> Parser error value
fail e =
    Parser <| \state -> Bad (Custom { at = state.offset } e)


{-| Run the given parser on the provided bytes and the result.

    import Bytes.Encode as E
    import Bytes.Parser as P


    E.string "hello"
        |> E.encode
        |> P.run (P.string 5)
    --> Ok "hello"


    E.string "hello"
        |> E.encode
        |> P.run (P.string 6)
    --> Err (P.OutOfBounds { at = 0, bytes = 6 })

-}
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


{-| Transform the value a parser produces

    import Bytes.Encode as E
    import Bytes.Parser as P


    E.string "hello"
        |> E.encode
        |> P.run (P.map String.length (P.string 5))
    --> Ok 5

-}
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


{-| Parse one thing, and then parse another thing based on the first thing.

This is very useful to make the content of your data drive your parser. As an
example, consider a string encoded as the length of the string, followed by the
actual data:

    import Bytes.Encode as E
    import Bytes.Parser as P exposing (Parser)


    string : Parser c e String
    string =
        P.unsignedInt8 |> P.andThen P.string


    [ E.unsignedInt8 5
    , E.string "hello"
    ]
        |> E.sequence
        |> E.encode
        |> P.run string
    --> Ok "hello"

-}
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


{-| Combine what 2 parsers produce into a single parser.

    import Bytes exposing (Bytes)
    import Bytes.Encode as E
    import Bytes.Parser as P exposing (Parser)


    input : Bytes
    input =
        [ E.unsignedInt8 3
        , E.string "wat"
        ]
            |> E.sequence
            |> E.encode


    map2Example : Parser c e String
    map2Example =
        P.map2 String.repeat P.unsignedInt8 (P.string 3)


    P.run map2Example input
    --> Ok "watwatwat"

Note that the effect of `map2` (and, in fact, every `map` variation) can also be
achieved using a combination of [`succeed`](#succeed) and [`keep`](#keep).

    equivalent : Parser c e String
    equivalent =
        P.succeed String.repeat
            |> P.keep P.unsignedInt8
            |> P.keep (P.string 3)

    P.run equivalent input
    --> Ok "watwatwat"

-}
map2 :
    (x -> y -> z)
    -> Parser error x
    -> Parser error y
    -> Parser error z
map2 f parserX parserY =
    parserX |> andThen (\x -> parserY |> andThen (\y -> succeed (f x y)))


{-| Keep the value produced by a parser in a pipeline.

Together with [`succeed`](#succeed) and [`ignore`](#ignore), this allows writing
pretty flexible parsers in a straightforward manner: the order in which things
are parsed is apparent.

    import Bytes.Encode as E
    import Bytes.Parser as P exposing (Parser)

    parser : Parser c e (Int, Int)
    parser =
        P.succeed Tuple.pair
            |> P.keep P.unsignedInt8
            |> P.ignore P.unsignedInt8
            |> P.keep P.unsignedInt8

    [ E.unsignedInt8 12
    , E.unsignedInt8 3
    , E.unsignedInt8 45
    ]
        |> E.sequence
        |> E.encode
        |> P.run parser
    --> Ok ( 12, 45 )

-}
keep :
    Parser error a
    -> Parser error (a -> b)
    -> Parser error b
keep val fun =
    map2 (<|) fun val


{-| Ignore the value produced by a parser.

Note that the parser must still succeed for the pipeline to succeed. This means
you can use this for checking the value of something, without using the value.

    import Bytes.Encode as E
    import Bytes.Parser as P exposing (Parser)


    type Error = Mismatch { expected : Int, actual : Int }


    match : Int -> Parser c Error Int
    match expected =
        P.unsignedInt8
            |> P.andThen
                (\actual ->
                    if expected == actual then
                        P.succeed actual
                    else
                        P.fail (Mismatch { expected = expected, actual = actual})
                )

    parser : Parser c Error ()
    parser =
        P.succeed ()
            |> P.ignore (match 66)


    E.unsignedInt8 66
        |> E.encode
        |> P.run parser
    --> Ok ()


    E.unsignedInt8 44
        |> E.encode
        |> P.run parser
    --> Mismatch { expected = 66, actual = 44 }
    -->   |> P.Custom { at = 1 }
    -->   |> Err

-}
ignore :
    Parser error ignore
    -> Parser error keep
    -> Parser error keep
ignore skipper keeper =
    map2 always keeper skipper


{-| Skip a number of bytes in a pipeline.

This is similar to `ignore`, but rather than parsing a value and discarding it,
this just goes ahead and skips them altogether.

-}
skip : Int -> Parser error value -> Parser error value
skip nBytes =
    ignore (bytes nBytes)


{-| Tries a bunch of parsers and succeeds with the first one to succeed.

Note that this uses backtracking when a parser fails after making some progress.

-}
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


{-| Represent the next step of a loop: Either continue looping with some new
internal state, or finish while producing a value.
-}
type Step state a
    = Loop state
    | Done a


{-| Loop a parser until it declares it is done looping.

The first argument is a function which, given some state, will usually parse
some stuff and indicate it wants to either continue, or declare it is done and
produce the final value. The second argument is the initial state for the loop.

This particular order of parameters was chosen to make it somewhat easier to
produce the initial state using a parser (which seems to be a fairly common use
case) and to hint at the mental model, which isn't unlike a `fold`.

    import Bytes.Encode as E
    import Bytes.Parser as P

    nullTerminatedString_ : (Int, P.Position) -> P.Parser c e (P.Step (Int, P.Position) String)
    nullTerminatedString_ ( count, startPos ) =
        P.unsignedInt8
            |> P.andThen
                (\byte ->
                     if byte == 0x00 then
                         P.string count
                             |> P.randomAccess { offset = 0, relativeTo = startPos }
                             |> P.map P.Done
                     else
                         P.succeed (P.Loop ( count + 1, startPos ))
                )

    nullTerminatedString : Parser c e String
    nullTerminatedString =
        P.map (Tuple.pair 0) P.position
            |> P.andThen (P.loop nullTerminatedString_)


    [ E.string "hello world!"
    , E.unsignedInt8 0
    ]
        |> E.sequence
        |> E.encode
        |> P.run nullTerminatedString
    --> Ok "hello world!"

-}
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


{-| Repeat a given parser `count` times.

The order of arguments is based on the common occurence of reading the number of
times to repeat something through a parser.

    import Bytes.Encode as E
    import Bytes.Parser as P


    intList : P.Parser c e (List Int)
    intList =
        P.unsignedInt8 |> P.andThen (P.repeat P.unsignedInt8)


    [ 5, 0, 1, 2, 3, 4 ]
        |> List.map E.unsignedInt8
        |> E.sequence
        |> E.encode
        |> P.run intList
    --> Ok [ 0, 1, 2, 3, 4 ]

-}
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
