module Main exposing (basic, loop, oneOf, repeat)

import Bytes as B
import Bytes.Decode as D
import Bytes.Decode.Branchable as P
import Bytes.Encode as E
import Expect
import Test exposing (..)


basic : Test
basic =
    describe "very basic stuff"
        [ test "basic unsignedInt8 works" <|
            \_ ->
                E.unsignedInt8 8
                    |> E.encode
                    |> P.run P.unsignedInt8
                    |> Expect.equal (Just 8)
        , test "no reading past end of input" <|
            \_ ->
                P.run P.unsignedInt8 emptyBytes
                    |> Expect.equal Nothing
        , test "succeed succeeds" <|
            \_ ->
                P.run (P.succeed "sure") emptyBytes
                    |> Expect.equal (Just "sure")
        , test "fail fails" <|
            \_ ->
                P.run P.fail emptyBytes
                    |> Expect.equal Nothing
        , test "can read multiple things" <|
            \_ ->
                encodeSequence
                    [ E.unsignedInt8 1
                    , E.unsignedInt8 2
                    ]
                    |> P.run (P.map2 Tuple.pair P.unsignedInt8 P.unsignedInt8)
                    |> Expect.equal (Just ( 1, 2 ))
        ]


loop : Test
loop =
    let
        parser : P.Decoder (List String)
        parser =
            P.unsignedInt8
                |> P.andThen (\cnt -> P.loop ( cnt, [] ) loopHelper)

        loopHelper :
            ( Int, List String )
            -> P.Decoder (D.Step ( Int, List String ) (List String))
        loopHelper ( cnt, acc ) =
            if cnt <= 0 then
                P.succeed (D.Done (List.reverse acc))

            else
                P.string 3
                    |> P.map (\s -> D.Loop ( cnt - 1, s :: acc ))
    in
    describe "loops"
        [ test "When everything goes well" <|
            \_ ->
                encodeSequence
                    [ E.unsignedInt8 3
                    , E.string "foo"
                    , E.string "bar"
                    , E.string "baz"
                    ]
                    |> P.run parser
                    -- |> Expect.equal Nothing
                    |> Expect.equal (Just [ "foo", "bar", "baz" ])
        , test "failure propagates" <|
            \_ ->
                encodeSequence
                    [ E.unsignedInt8 3
                    , E.string "foo"
                    , E.string "bar"
                    ]
                    |> P.run parser
                    |> Expect.equal Nothing
        ]


repeat : Test
repeat =
    test "repeat repeats" <|
        \_ ->
            let
                parser : P.Decoder (List String)
                parser =
                    P.andThen (\count -> P.repeat count (P.string 3)) P.unsignedInt8
            in
            encodeSequence
                [ E.unsignedInt8 3
                , E.string "foo"
                , E.string "bar"
                , E.string "baz"
                ]
                |> P.run parser
                |> Expect.equal (Just [ "foo", "bar", "baz" ])


oneOf : Test
oneOf =
    let
        parseExactString : String -> P.Decoder String
        parseExactString str =
            P.string (String.length str)
                |> P.andThen
                    (\s ->
                        if s == str then
                            P.succeed str

                        else
                            P.fail
                    )

        oneOfFooBarBaz =
            P.oneOf
                [ parseExactString "foo"
                , parseExactString "bar"
                , parseExactString "baz"
                ]
    in
    describe "oneOf"
        [ test "none" <|
            \_ ->
                P.run oneOfFooBarBaz emptyBytes
                    |> Expect.equal Nothing
        , test "foo" <|
            \_ ->
                P.run oneOfFooBarBaz (E.encode <| E.string "foo...")
                    |> Expect.equal (Just "foo")
        , test "bar" <|
            \_ ->
                P.run oneOfFooBarBaz (E.encode <| E.string "bar...")
                    |> Expect.equal (Just "bar")
        , test "foobarbaz" <|
            \_ ->
                P.run (P.repeat 3 oneOfFooBarBaz) (E.encode <| E.string "foobarbaz...")
                    |> Expect.equal (Just [ "foo", "bar", "baz" ])
        ]


encodeSequence : List E.Encoder -> B.Bytes
encodeSequence =
    E.sequence >> E.encode


emptyBytes : B.Bytes
emptyBytes =
    encodeSequence []
