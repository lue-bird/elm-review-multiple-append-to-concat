module MultipleAppendToConcatTest exposing (all)

import MultipleAppendToConcat exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "MultipleAppendToConcat"
        [ test "should not report an error when REPLACEME" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run (rule MultipleAppendToConcat.ApplyList)
                    |> Review.Test.expectNoErrors
        , test "should report an error when REPLACEME" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run (rule MultipleAppendToConcat.ApplyList)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "REPLACEME"
                            }
                        ]
        ]
