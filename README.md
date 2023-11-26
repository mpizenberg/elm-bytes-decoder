# elm-bytes-decoder 

> Enable branching (`oneOf`) for elm/bytes decoders.

This package builds on top of `elm/bytes#Bytes.Decode` and adds
functionality to branch decoders with `oneOf`.

It is heavily inspired by `zwilias/elm-bytes-parser` and even starts as a fork of it.
The key changes are two-fold.

First, on the visible side, this package has removed all context and error handling
in order to stay as close as possible to `elm/bytes` decoders.

And second, the decoding is handled slightly differently.
In `zwilias/elm-bytes-parser`, for every decoder,
the code does a double decoding consisting in an bytes offset followed by the actual decoder:

```elm
-- extract from zwilias/elm-bytes-parser
fromDecoder : Decoder v -> Int -> Parser context error v
fromDecoder dec byteLength =
    Parser <|
        \state ->
            let
                combined =
                    Decode.map2 (always identity) (Decode.bytes state.offset) dec
            in
            case Decode.decode combined state.input of
                Just res ->
                    Good res { state | offset = state.offset + byteLength }

                Nothing ->
                    Bad (OutOfBounds { at = state.offset, bytes = byteLength })
```

In contrast, in this `elm-bytes-decoder` package, decoders are left unchanged.

```elm
-- extract from mpizenberg/elm-bytes-decoder
fromDecoder : D.Decoder v -> Int -> Decoder v
fromDecoder decoder byteLength =
    Decoder <|
        \state ->
            D.map
                (\v -> ( { input = state.input, offset = state.offset + byteLength }, v ))
                decoder
```

This is enabled by the state storing a decoder instead of a fully resolved decoding result.

```elm
type Decoder value
    = Decoder (State -> D.Decoder ( State, value ))

type alias State =
    { input : Bytes, offset : Int }
```

Thanks to this change, we can compose decoders as usual, and only when calling `oneOf`,
backtracking is taken care of, instead of for each decoder.
In theory, this should result in more performant decoding.
In practice, I didn't notice any significant change.