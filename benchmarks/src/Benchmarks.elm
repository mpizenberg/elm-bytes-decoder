module Benchmarks exposing (main)

import Benchmark exposing (Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BytesBenchmarks
import BranchableBenchmarks


main : BenchmarkProgram
main =
    program <|
        Benchmark.describe ("elm/bytes vs mpizenberg/elm-bytes-decoder")
            [ Benchmark.compare
                "Decode list with 10 floats"
                "elm/bytes"
                BytesBenchmarks.decodeList
                "mpizenberg/elm-bytes-decoder"
                BranchableBenchmarks.decodeList
            , Benchmark.compare
                "Decode list with 1000 floats"
                "elm/bytes"
                BytesBenchmarks.decodeLongList
                "mpizenberg/elm-bytes-decoder"
                BranchableBenchmarks.decodeLongList
            ]
