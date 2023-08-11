# elm-review-multiple-append-to-concat

Provides the [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule
[`MultipleAppendToConcat`](https://package.elm-lang.org/packages/lue-bird/elm-review-multiple-append-to-concat/1.0.0/MultipleAppendToConcat)
that replaces multiple `++` in sequence with concat, like

```elm
combinedString =
    "Your name is "
        ++ (nameParts
                |> String.join " "
           )
        ++ " and your age is "
        ++ (age |> String.fromInt)
        ++ "."
```
fixed to
```elm
combinedString =
    String.concat
        [ "Your name is "
        , nameParts
            |> String.join " "
        , " and your age is "
        , age |> String.fromInt
        , "."
        ]
```

and for lists

```elm
combinedList =
    a
        ++ [ b, c ]
        ++ d
```
fixed to
```elm
combinedList =
    List.concat
        [ a
        , [ b, c ]
        , d
        ]
```

  - `elm-review` doesn't have type inference, so for values like `a ++ b ++ c` this rule doesn't provide a fix
  - It is highly, highly recommended to use [`elm-review-simplify`](https://dark.elm.dmy.fr/packages/jfmengels/elm-review-simplify/latest/) alongside which simplifies
    `[ a ] ++ b ++ c` to `a :: b ++ c`,
    `[ a, b ] ++ [ c, d ]` to `[ a, b, c, d ]`,
    `a :: [ b, c ] ++ d` to `[ a, b, c ] ++ d` etc

## why

#### it's more readable

This is somewhat subjective. Make sure everyone thinks the same before enabling the rule.

```elm
someDeclaration =
    let
        combinedList =
            (a
                ++ ([ b, c ]
                        |> List.map .name
                   )
                ++ d
            )
                |> String.join ", "
    in
    ...
```
vs
```elm
someDeclaration =
    let
        combinedList =
            [ a
            , [ b, c ]
                |> List.map .name
            , d
            ]
                |> List.concat
                |> String.join ", "
    in
    ...
```

#### less `appendable` and infix operators
`String.concat` and `List.concat` take concrete types and don't have the "magic-ness" of constrained type variables, operator precedence etc.


## try it out

```bash
elm-review --template lue-bird/elm-review-multiple-append-to-concat/example
```

If you like what it does, install it to your `review/` project and

## configure it

```elm
module ReviewConfig exposing (config)

import MultipleAppendToConcat
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ MultipleAppendToConcat.rule MultipleAppendToConcat.ApplyList
    ]
```
