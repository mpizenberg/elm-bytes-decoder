module BranchableBenchmarks exposing (..)

import Bytes.Decode.Branchable as D exposing (Decoder)
import Bytes.Decode exposing (Step(..))
import Data


decodeList : () -> Maybe (List Float)
decodeList =
    \_ -> D.decode parseFloatList Data.bytesFloatList


decodeLongList : () -> Maybe (List Float)
decodeLongList =
    \_ -> D.decode parseFloatList Data.longBytesFloatList


parseFloatList : Decoder (List Float)
parseFloatList =
    list (D.float64 Data.endianness)


list : Decoder  a -> Decoder (List a)
list decoder =
    D.unsignedInt32 Data.endianness
        |> D.andThen (D.repeat decoder)