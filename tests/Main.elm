module Main exposing (basic, loop, oneOf, repeat)

import Bytes as B
import Bytes.Decode
import Bytes.Decode.Branchable as D
import Bytes.Encode as E
import Expect
import Test exposing (Test, describe, test)


basic : Test
basic =
    describe "very basic stuff"
        [ test "basic unsignedInt8 works" <|
            \_ ->
                E.unsignedInt8 8
                    |> E.encode
                    |> D.run D.unsignedInt8
                    |> Expect.equal (Just 8)
        , test "no reading past end of input" <|
            \_ ->
                D.run D.unsignedInt8 emptyBytes
                    |> Expect.equal Nothing
        , test "succeed succeeds" <|
            \_ ->
                D.run (D.succeed "sure") emptyBytes
                    |> Expect.equal (Just "sure")
        , test "fail fails" <|
            \_ ->
                D.run D.fail emptyBytes
                    |> Expect.equal Nothing
        , test "can read multiple things" <|
            \_ ->
                encodeSequence
                    [ E.unsignedInt8 1
                    , E.unsignedInt8 2
                    ]
                    |> D.run (D.map2 Tuple.pair D.unsignedInt8 D.unsignedInt8)
                    |> Expect.equal (Just ( 1, 2 ))
        ]


loop : Test
loop =
    let
        parser : D.Decoder (List String)
        parser =
            D.unsignedInt8
                |> D.andThen (\cnt -> D.loop ( cnt, [] ) loopHelper)

        loopHelper :
            ( Int, List String )
            -> D.Decoder (Bytes.Decode.Step ( Int, List String ) (List String))
        loopHelper ( cnt, acc ) =
            if cnt <= 0 then
                D.succeed (Bytes.Decode.Done (List.reverse acc))

            else
                D.string 3
                    |> D.map (\s -> Bytes.Decode.Loop ( cnt - 1, s :: acc ))
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
                    |> D.run parser
                    -- |> Expect.equal Nothing
                    |> Expect.equal (Just [ "foo", "bar", "baz" ])
        , test "failure propagates" <|
            \_ ->
                encodeSequence
                    [ E.unsignedInt8 3
                    , E.string "foo"
                    , E.string "bar"
                    ]
                    |> D.run parser
                    |> Expect.equal Nothing
        ]


repeat : Test
repeat =
    test "repeat repeats" <|
        \_ ->
            let
                parser : D.Decoder (List String)
                parser =
                    D.unsignedInt8
                        |> D.andThen (D.repeat (D.string 3))
            in
            encodeSequence
                [ E.unsignedInt8 3
                , E.string "foo"
                , E.string "bar"
                , E.string "baz"
                ]
                |> D.run parser
                |> Expect.equal (Just [ "foo", "bar", "baz" ])


oneOf : Test
oneOf =
    let
        parseExactString : String -> D.Decoder String
        parseExactString str =
            D.string (String.length str)
                |> D.andThen
                    (\s ->
                        if s == str then
                            D.succeed str

                        else
                            D.fail
                    )

        oneOfFooBarBaz : D.Decoder String
        oneOfFooBarBaz =
            D.oneOf
                [ parseExactString "foo"
                , parseExactString "bar"
                , parseExactString "baz"
                ]
    in
    describe "oneOf"
        [ test "none" <|
            \_ ->
                D.run oneOfFooBarBaz emptyBytes
                    |> Expect.equal Nothing
        , test "foo" <|
            \_ ->
                D.run oneOfFooBarBaz (E.encode <| E.string "foo...")
                    |> Expect.equal (Just "foo")
        , test "bar" <|
            \_ ->
                D.run oneOfFooBarBaz (E.encode <| E.string "bar...")
                    |> Expect.equal (Just "bar")
        , test "foobarbaz" <|
            \_ ->
                D.run (D.repeat oneOfFooBarBaz 3) (E.encode <| E.string "foobarbaz...")
                    |> Expect.equal (Just [ "foo", "bar", "baz" ])
        ]


encodeSequence : List E.Encoder -> B.Bytes
encodeSequence =
    E.sequence >> E.encode


emptyBytes : B.Bytes
emptyBytes =
    encodeSequence []
