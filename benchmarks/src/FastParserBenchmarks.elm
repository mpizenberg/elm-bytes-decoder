module FastParserBenchmarks exposing (..)

import Bytes.FastParser as Parser exposing (Parser)
import Bytes.Decode exposing (Step(..))
import Data


decodeList : () -> Maybe (List Float)
decodeList =
    \_ -> Parser.run parseFloatList Data.bytesFloatList


decodeLongList : () -> Maybe (List Float)
decodeLongList =
    \_ -> Parser.run parseFloatList Data.longBytesFloatList


parseFloatList : Parser (List Float)
parseFloatList =
    list (Parser.float64 Data.endianness)


list : Parser  a -> Parser (List a)
list decoder =
    Parser.unsignedInt32 Data.endianness
        |> Parser.andThen (\count -> Parser.repeat count decoder)