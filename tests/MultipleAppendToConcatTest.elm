module MultipleAppendToConcatTest exposing (all)

import MultipleAppendToConcat exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "MultipleAppendToConcat"
        [ test "should not report an error when there is only one ++" <|
            \() ->
                """module A exposing (..)
a = "a" ++ "bc"
b = [ 1 ] ++ [ 2, 3 ]
"""
                    |> Review.Test.run (rule MultipleAppendToConcat.ApplyList)
                    |> Review.Test.expectNoErrors
        , test "should not report an error when ++ is nested in operand of ++" <|
            \() ->
                """module A exposing (..)
a = "a" ++ ("b" ++ "c")
b = [ 1 ] ++ ([ 2 ] ++ [ 3 ])
"""
                    |> Review.Test.run (rule MultipleAppendToConcat.ApplyList)
                    |> Review.Test.expectNoErrors
        , test "should report an error but not provide a fix for non-obvious appendable typed values" <|
            \() ->
                """module A exposing (..)
a = b ++ c ++ d
"""
                    |> Review.Test.run (rule MultipleAppendToConcat.ApplyList)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "multiple `++` in sequence can be replaced with concat"
                            , details = [ "Putting all the appended values in a list and combining them with String.concat or List.concat is more readable. A more detailed explanation can be found at https://package.elm-lang.org/packages/lue-bird/elm-review-multiple-append-to-concat/latest#why" ]
                            , under = "b ++ c ++ d"
                            }
                        ]
        , test "should fix multiple ++ strings single-line with apply" <|
            \() ->
                """module A exposing (..)
a = "b" ++ "c" ++ "d"
"""
                    |> Review.Test.run (rule MultipleAppendToConcat.ApplyList)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "multiple `++` in sequence can be replaced with concat"
                            , details = [ "Putting all the appended values in a list and combining them with String.concat or List.concat is more readable. A more detailed explanation can be found at https://package.elm-lang.org/packages/lue-bird/elm-review-multiple-append-to-concat/latest#why" ]
                            , under = "\"b\" ++ \"c\" ++ \"d\""
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = String.concat [ "b", "c", "d" ]
"""
                        ]
        , test "should fix multiple ++ strings multi-line with apply" <|
            \() ->
                """module A exposing (..)
a =
    "b"
        ++ "c"
        ++ "d"
"""
                    |> Review.Test.run (rule MultipleAppendToConcat.ApplyList)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "multiple `++` in sequence can be replaced with concat"
                            , details = [ "Putting all the appended values in a list and combining them with String.concat or List.concat is more readable. A more detailed explanation can be found at https://package.elm-lang.org/packages/lue-bird/elm-review-multiple-append-to-concat/latest#why" ]
                            , under = """"b"
        ++ "c"
        ++ "d\""""
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a =
    String.concat
    [ "b"
    , "c"
    , "d" ]
"""
                        ]
        ]
