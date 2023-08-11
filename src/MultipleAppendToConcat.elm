module MultipleAppendToConcat exposing (rule, ListSupplyStyle(..))

{-| Rule: Replace multiple `++`s in sequence with concat.

@docs rule, ListSupplyStyle

-}

import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Rule)


{-| In what style will `[ ... ]` be supplied to `List.concat`?

  - `ApplyList`: `List.concat [ ... ]`
  - `PipeRightList`: `[ ... ] |> List.concat`
  - `PipeLeftList`: `List.concat <| [ ... ]`

-}
type ListSupplyStyle
    = ApplyList
    | PipeRightList
    | PipeLeftList


{-| Replaces multiple `++`s in sequence with concat in a given [`ListSupplyStyle`](#ListSupplyStyle).

For example, running `MultipleAppendToConcat.rule MultipleAppendToConcat.ApplyList` on

    combinedString =
        "Your name is "
            ++ (nameParts
                    |> String.join " "
               )
            ++ " and your age is "
            ++ (age |> String.fromInt)
            ++ "."

is fixed to

    combinedString =
        String.concat
            [ "Your name is "
            , nameParts
                |> String.join " "
            , " and your age is "
            , age |> String.fromInt
            , "."
            ]

and for lists

    combinedList =
        a
            ++ [ b, c ]
            ++ d

fixed to

    combinedList =
        List.concat
            [ a
            , [ b, c ]
            , d
            ]

`elm-review` doesn't have type inference, so for values like `a ++ b ++ c` this rule doesn't provide a fix.

Read the [readme for why you would (not) want to enable this rule](https://package.elm-lang.org/packages/lue-bird/elm-review-multiple-append-to-concat/1.0.0#why).

-}
rule : ListSupplyStyle -> Rule
rule listSupplyStyle =
    Rule.newModuleRuleSchemaUsingContextCreator "MultipleAppendToConcat" initialContext
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    {}


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\() ->
            {}
        )


expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        _ ->
            ( [], context )
