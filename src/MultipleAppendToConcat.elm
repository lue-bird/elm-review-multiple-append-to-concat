module MultipleAppendToConcat exposing (rule, ListSupplyStyle(..))

{-| Rule: Replace multiple `++` in sequence with concat.

@docs rule, ListSupplyStyle

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Review.Fix as Fix exposing (Fix)
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


{-| Replaces multiple `++` in sequence with concat in a given [`ListSupplyStyle`](#ListSupplyStyle).

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
        |> Rule.providesFixesForModuleRule
        |> Rule.withExpressionEnterVisitor
            (\expressionNode context ->
                ( expressionVisitor
                    { expressionNode = expressionNode
                    , context = context
                    , listSupplyStyle = listSupplyStyle
                    }
                , context
                )
            )
        |> Rule.fromModuleRuleSchema


type alias Context =
    {}


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\() ->
            {}
        )


expressionVisitor :
    { expressionNode : Node Expression
    , context : Context
    , listSupplyStyle : ListSupplyStyle
    }
    -> List (Rule.Error {})
expressionVisitor info =
    let
        appendable : { type_ : Maybe AppendableType, operandRanges : List Range }
        appendable =
            info.expressionNode |> toAppendable
    in
    case appendable.operandRanges of
        appendOperand0Range :: appendOperand1Range :: appendOperand2Range :: appendOperand3RangeUp ->
            [ Rule.errorWithFix
                { message = "multiple `++` in sequence can be replaced with concat"
                , details = [ "Putting all the appended values in a list and combining them with String.concat or List.concat is more readable. A more detailed explanation can be found at https://package.elm-lang.org/packages/lue-bird/elm-review-multiple-append-to-concat/latest#why" ]
                }
                (info.expressionNode |> Node.range)
                (case appendable.type_ of
                    Nothing ->
                        []

                    Just appendableType ->
                        appendSequenceToConcatListFix
                            { appendableType = appendableType
                            , appendOperands = appendOperand0Range :: appendOperand1Range :: appendOperand2Range :: appendOperand3RangeUp
                            , structure = info.expressionNode |> Node.range
                            , style = info.listSupplyStyle
                            }
                )
            ]

        _ ->
            []


type AppendableType
    = AppendableString
    | AppendableList


appendSequenceToConcatListFix :
    { appendableType : AppendableType
    , appendOperands : List Range
    , style : ListSupplyStyle
    , structure : Range
    }
    -> List Fix
appendSequenceToConcatListFix config =
    let
        appendableConcatString : String
        appendableConcatString =
            case config.appendableType of
                AppendableString ->
                    "String.concat"

                AppendableList ->
                    "List.concat"

        multiLineBreak : String
        multiLineBreak =
            [ "\n", String.repeat (config.structure.start.column - 1) " " ] |> String.concat

        linesWithoutListBracketOrComma : List Int
        linesWithoutListBracketOrComma =
            List.range (config.structure.start.row + 1) config.structure.end.row
                |> List.filter
                    (\row ->
                        betweenOperands
                            |> List.all
                                (\betweenRange -> not (row |> rowIsInRange betweenRange))
                    )

        betweenOperands : List { start : Elm.Syntax.Range.Location, end : Elm.Syntax.Range.Location }
        betweenOperands =
            config.appendOperands
                |> consecutiveMap
                    (\appendOperandRange ->
                        { start = appendOperandRange.previous.end
                        , end = appendOperandRange.current.start
                        }
                    )

        multiLineCommaSeparatedOperandsIndentedBy : Int -> List Fix
        multiLineCommaSeparatedOperandsIndentedBy indentation =
            betweenOperands
                |> List.map
                    (\between2Operands ->
                        Fix.replaceRangeBy between2Operands
                            ([ multiLineBreak, String.repeat indentation " ", ", " ] |> String.concat)
                    )

        singleLineCommaSeparatedOperands : List Fix
        singleLineCommaSeparatedOperands =
            betweenOperands
                |> List.map
                    (\between2Operands ->
                        Fix.replaceRangeBy between2Operands ", "
                    )
    in
    case config.style of
        ApplyList ->
            case config.structure |> lineSpan of
                SingleLine ->
                    [ Fix.insertAt config.structure.start (appendableConcatString ++ " [ ")
                    , Fix.insertAt config.structure.end " ]"
                    ]
                        ++ singleLineCommaSeparatedOperands

                MultiLine ->
                    [ Fix.insertAt config.structure.start (appendableConcatString ++ multiLineBreak ++ "    [ ")
                    , Fix.insertAt config.structure.end (multiLineBreak ++ "    ]")
                    ]
                        ++ multiLineCommaSeparatedOperandsIndentedBy 4
                        ++ (linesWithoutListBracketOrComma
                                |> List.map (\row -> Fix.insertAt { row = row, column = 1 } "    ")
                           )

        PipeLeftList ->
            case config.structure |> lineSpan of
                SingleLine ->
                    [ Fix.insertAt config.structure.start
                        ([ "(", appendableConcatString, " <| [ " ] |> String.concat)
                    , Fix.insertAt config.structure.end " ])"
                    ]
                        ++ singleLineCommaSeparatedOperands

                MultiLine ->
                    [ Fix.insertAt config.structure.start
                        ([ "(", appendableConcatString, " <|", multiLineBreak, "    [ " ] |> String.concat)
                    , Fix.insertAt config.structure.end
                        ([ multiLineBreak, "    ]", multiLineBreak, ")" ] |> String.concat)
                    ]
                        ++ multiLineCommaSeparatedOperandsIndentedBy 4
                        ++ (linesWithoutListBracketOrComma
                                |> List.map (\row -> Fix.insertAt { row = row, column = 1 } "    ")
                           )

        PipeRightList ->
            case config.structure |> lineSpan of
                SingleLine ->
                    [ Fix.insertAt config.structure.start "([ "
                    , Fix.insertAt config.structure.end
                        ([ " ] |> "
                         , appendableConcatString
                         , ")"
                         ]
                            |> String.concat
                        )
                    ]
                        ++ singleLineCommaSeparatedOperands

                MultiLine ->
                    [ Fix.insertAt config.structure.start "([ "
                    , Fix.insertAt config.structure.end
                        ([ multiLineBreak
                         , " ]"
                         , multiLineBreak
                         , "    |> "
                         , appendableConcatString
                         , multiLineBreak
                         , ")"
                         ]
                            |> String.concat
                        )
                    ]
                        ++ multiLineCommaSeparatedOperandsIndentedBy 1
                        ++ (linesWithoutListBracketOrComma
                                |> List.map (\row -> Fix.insertAt { row = row, column = 1 } " ")
                           )


toAppendable :
    Node Expression
    ->
        { type_ : Maybe AppendableType
        , operandRanges : List Range
        }
toAppendable (Node expressionRange expression) =
    case expression of
        Expression.OperatorApplication "++" _ left right ->
            let
                leftAppendable : { type_ : Maybe AppendableType, operandRanges : List Range }
                leftAppendable =
                    left |> toAppendable

                rightAppendable : { type_ : Maybe AppendableType, operandRanges : List Range }
                rightAppendable =
                    right |> toAppendable
            in
            { type_ = leftAppendable.type_ |> onNothing rightAppendable.type_
            , operandRanges = leftAppendable.operandRanges ++ rightAppendable.operandRanges
            }

        Expression.Literal _ ->
            { type_ = AppendableString |> Just, operandRanges = [ expressionRange ] }

        Expression.OperatorApplication "::" _ _ _ ->
            { type_ = AppendableList |> Just, operandRanges = [ expressionRange ] }

        Expression.ListExpr _ ->
            { type_ = AppendableList |> Just, operandRanges = [ expressionRange ] }

        _ ->
            { type_ = Nothing, operandRanges = [ expressionRange ] }


type LineSpan
    = SingleLine
    | MultiLine


lineSpan : Range -> LineSpan
lineSpan =
    \range ->
        case range.end.row - range.start.row of
            0 ->
                SingleLine

            _ ->
                MultiLine


rowIsInRange : Range -> (Int -> Bool)
rowIsInRange range =
    \row ->
        (row >= range.start.row)
            && (row <= range.end.row)


onNothing : Maybe a -> (Maybe a -> Maybe a)
onNothing secondTry =
    \firstTry ->
        case firstTry of
            Nothing ->
                secondTry

            Just firstTryContent ->
                firstTryContent |> Just


consecutiveMap : ({ previous : element, current : element } -> newElement) -> (List element -> List newElement)
consecutiveMap previousAndCurrentToNewElement =
    \list ->
        List.map2
            (\current previous ->
                previousAndCurrentToNewElement { previous = previous, current = current }
            )
            (list |> List.drop 1)
            list
