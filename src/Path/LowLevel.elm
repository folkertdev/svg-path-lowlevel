module Path.LowLevel
    exposing
        ( ArcFlag(..)
        , Coordinate
        , Direction(..)
        , DrawTo(..)
        , EllipticalArcArgument
        , Mode(..)
        , MoveTo(..)
        , SubPath
        , Option
        , decodeFlags
        , encodeFlags
        , toString
        , toStringWith
        , decimalPlaces
        )

{-| A low-level package for working with svg path strings

This package has two use cases

  - **convert Elm data into svg paths**
  - **convert svg path syntax into elm data**

It is meant as a foundation: there is little type safety and convenience.
It's just a literal translation of the SVG spec into elm data types.

@docs toString,toStringWith, Option, decimalPlaces
@docs SubPath, Mode, MoveTo, DrawTo, Coordinate

@docs ArcFlag, Direction
@docs EllipticalArcArgument, encodeFlags, decodeFlags

-}

import Char


{-| A subpath is a `MoveTo` followed by a list of `DrawTo`s
-}
type alias SubPath =
    { moveto : MoveTo, drawtos : List DrawTo }


{-| Represent a point in 2D space with a tuple of floats
-}
type alias Coordinate =
    ( Float, Float )


{-| The mode of an instruction
-}
type Mode
    = Relative
    | Absolute


{-| MoveTo instructions move the cursor, but don't draw anything.
-}
type MoveTo
    = MoveTo Mode Coordinate


{-| Constructors for DrawTo instructions
-}
type DrawTo
    = LineTo Mode (List Coordinate)
    | Horizontal Mode (List Float)
    | Vertical Mode (List Float)
    | CurveTo Mode (List ( Coordinate, Coordinate, Coordinate ))
    | SmoothCurveTo Mode (List ( Coordinate, Coordinate ))
    | QuadraticBezierCurveTo Mode (List ( Coordinate, Coordinate ))
    | SmoothQuadraticBezierCurveTo Mode (List Coordinate)
    | EllipticalArc Mode (List EllipticalArcArgument)
    | ClosePath


{-| The arguments for an Arc
-}
type alias EllipticalArcArgument =
    { radii : ( Float, Float )
    , xAxisRotate : Float
    , arcFlag : ArcFlag
    , direction : Direction
    , target : Coordinate
    }


{-| Corresponds to a sweep flag of 1
-}
clockwise : Direction
clockwise =
    Clockwise


{-| Corresponds to a sweep flag of 0
-}
counterClockwise : Direction
counterClockwise =
    CounterClockwise


{-| Corresponds to an arc flag of 1
-}
largestArc : ArcFlag
largestArc =
    LargestArc


{-| Corresponds to an arc flag of 0
-}
smallestArc : ArcFlag
smallestArc =
    SmallestArc


{-| Determine which arc to draw
-}
type ArcFlag
    = SmallestArc
    | LargestArc


{-| Determine which arc to draw
-}
type Direction
    = Clockwise
    | CounterClockwise


{-| Turn the flags into numbers

    case arcFlag of
        LargestArc -> 1
        SmallestArc -> 0

    case direction of
        Clockwise -> 0
        CounterClockwise -> 1

-}
encodeFlags : ( ArcFlag, Direction ) -> ( Int, Int )
encodeFlags ( arcFlag, direction ) =
    case ( arcFlag, direction ) of
        ( LargestArc, Clockwise ) ->
            ( 1, 0 )

        ( SmallestArc, Clockwise ) ->
            ( 0, 0 )

        ( LargestArc, CounterClockwise ) ->
            ( 1, 1 )

        ( SmallestArc, CounterClockwise ) ->
            ( 0, 1 )


{-| Try to decode two ints into flag values. Inverse of `encodeFlags`.
-}
decodeFlags : ( Int, Int ) -> Maybe ( ArcFlag, Direction )
decodeFlags ( arcFlag, sweepFlag ) =
    case ( arcFlag, sweepFlag ) of
        ( 1, 0 ) ->
            Just ( LargestArc, Clockwise )

        ( 0, 0 ) ->
            Just ( SmallestArc, Clockwise )

        ( 1, 1 ) ->
            Just ( LargestArc, CounterClockwise )

        ( 0, 1 ) ->
            Just ( SmallestArc, CounterClockwise )

        _ ->
            Nothing



-- toString conversion


{-| Convert a list of subpaths to svg path syntax

    myPath : List SubPath
    myPath =
        [ { moveto = MoveTo Relative ( 10, 20 )
          , drawtos =
                [ EllipticalArc Absolute
                    [ { radii = ( 25, 25 )
                    , xAxisRotate = -30
                    , arcFlag = SmallestArc
                    , direction = CounterClockwise
                    , target = ( 50, -25 )
                    }
                    ]
                ]
          }
        ]

    Path.LowLevel.toString myPath
        --> "m10,20 A25,25 -30 0 1 50,-25"

-}
toString : List SubPath -> String
toString =
    toStringWith []


{-| Formatting options
-}
type Option
    = DecimalPlaces Int


{-| Set the maximum number of decimal places in the output string

    myPath : List SubPath
    myPath =
        [ { moveto = MoveTo Relative ( 10, 20 )
          , drawtos =
              [ LineTo Absolute
                [ ( 1 / 3, 1 / 3 )
                , ( 1 / 7, 1 / 7 )
                ]
              ]
          }
        ]

    toStringWith [] myPath
        --> "m10,20 L0.3333333333333333,0.3333333333333333 0.14285714285714285,0.14285714285714285"

    toStringWith [ decimalPlaces 3 ] myPath
        --> "m10,20 L0.333,0.333 0.143,0.143"

-}
decimalPlaces : Int -> Option
decimalPlaces =
    DecimalPlaces


type alias Config =
    { floatFormatter : Float -> String }


defaultConfig : Config
defaultConfig =
    { floatFormatter = String.fromFloat }


optionFolder : Option -> Config -> Config
optionFolder option config =
    case option of
        DecimalPlaces n ->
            { config | floatFormatter = roundTo n }


accumulateOptions : List Option -> Config
accumulateOptions =
    List.foldl optionFolder defaultConfig


{-| Convert a list of subpaths to a string, with some extra formatting options
-}
toStringWith : List Option -> List SubPath -> String
toStringWith options subpaths =
    let
        config =
            accumulateOptions options
    in
        subpaths
            |> List.map (toStringSubPath config)
            |> String.join " "


toStringSubPath : Config -> SubPath -> String
toStringSubPath config { moveto, drawtos } =
    stringifyMoveTo config moveto ++ " " ++ String.join " " (List.map (stringifyDrawTo config) drawtos)


stringifyMoveTo : Config -> MoveTo -> String
stringifyMoveTo config (MoveTo mode coordinate) =
    case mode of
        Absolute ->
            "M" ++ stringifyCoordinate config coordinate

        Relative ->
            "m" ++ stringifyCoordinate config coordinate


stringifyDrawTo : Config -> DrawTo -> String
stringifyDrawTo config command =
    if isEmpty command then
        ""
    else
        case command of
            LineTo mode coordinates ->
                stringifyCharacter mode 'L' ++ String.join " " (List.map (stringifyCoordinate config) coordinates)

            Horizontal mode coordinates ->
                if List.isEmpty coordinates then
                    ""
                else
                    stringifyCharacter mode 'H' ++ String.join " " (List.map String.fromFloat coordinates)

            Vertical mode coordinates ->
                stringifyCharacter mode 'V' ++ String.join " " (List.map String.fromFloat coordinates)

            CurveTo mode coordinates ->
                stringifyCharacter mode 'C' ++ String.join " " (List.map (stringifyCoordinate3 config) coordinates)

            SmoothCurveTo mode coordinates ->
                stringifyCharacter mode 'S' ++ String.join " " (List.map (stringifyCoordinate2 config) coordinates)

            QuadraticBezierCurveTo mode coordinates ->
                stringifyCharacter mode 'Q' ++ String.join " " (List.map (stringifyCoordinate2 config) coordinates)

            SmoothQuadraticBezierCurveTo mode coordinates ->
                stringifyCharacter mode 'T' ++ String.join " " (List.map (stringifyCoordinate config) coordinates)

            EllipticalArc mode arguments ->
                stringifyCharacter mode 'A' ++ String.join " " (List.map (stringifyEllipticalArcArgument config) arguments)

            ClosePath ->
                "Z"


{-| Checks that there is an actual command in the drawto

A path string like "L" without arguments is invalid, but not ruled out by the types, so we have
to do this check when converting to string in order to ensure valid path syntax

-}
isEmpty : DrawTo -> Bool
isEmpty command =
    case command of
        LineTo mode coordinates ->
            List.isEmpty coordinates

        Horizontal mode coordinates ->
            List.isEmpty coordinates

        Vertical mode coordinates ->
            List.isEmpty coordinates

        CurveTo mode coordinates ->
            List.isEmpty coordinates

        SmoothCurveTo mode coordinates ->
            List.isEmpty coordinates

        QuadraticBezierCurveTo mode coordinates ->
            List.isEmpty coordinates

        SmoothQuadraticBezierCurveTo mode coordinates ->
            List.isEmpty coordinates

        EllipticalArc mode arguments ->
            List.isEmpty arguments

        ClosePath ->
            False


stringifyEllipticalArcArgument : Config -> EllipticalArcArgument -> String
stringifyEllipticalArcArgument config { radii, xAxisRotate, arcFlag, direction, target } =
    let
        ( arc, sweep ) =
            encodeFlags ( arcFlag, direction )
    in
        String.join " "
            [ stringifyCoordinate config radii
            , String.fromFloat xAxisRotate
            , String.fromInt arc
            , String.fromInt sweep
            , stringifyCoordinate config target
            ]


stringifyCharacter : Mode -> Char -> String
stringifyCharacter mode character =
    case mode of
        Absolute ->
            String.fromChar (Char.toUpper character)

        Relative ->
            String.fromChar (Char.toLower character)


stringifyCoordinate : Config -> Coordinate -> String
stringifyCoordinate config ( x, y ) =
    config.floatFormatter x ++ "," ++ config.floatFormatter y


stringifyCoordinate2 : Config -> ( Coordinate, Coordinate ) -> String
stringifyCoordinate2 config ( c1, c2 ) =
    stringifyCoordinate config c1 ++ " " ++ stringifyCoordinate config c2


stringifyCoordinate3 : Config -> ( Coordinate, Coordinate, Coordinate ) -> String
stringifyCoordinate3 config ( c1, c2, c3 ) =
    stringifyCoordinate config c1 ++ " " ++ stringifyCoordinate config c2 ++ " " ++ stringifyCoordinate config c3


floatFullConfig : Float -> String
floatFullConfig =
    String.fromFloat


floatWithDecimals : Int -> Float -> String
floatWithDecimals n num =
    roundTo n num


{-| Rounds a float to given number of decimal points.

taken from [elm-formatting]

[elm-formatting]: https://github.com/amitu/elm-formatting/blob/1.0.0/src/Formatting.elm

-}
roundTo : Int -> Float -> String
roundTo n value =
    if n == 0 then
        String.fromInt (round value)
    else
        let
            exp =
                10 ^ n

            raised =
                abs (round (value * toFloat exp))

            sign =
                if value < 0.0 then
                    "-"
                else
                    ""

            decimals =
                Basics.remainderBy exp raised
        in
            if decimals == 0 then
                sign ++ String.fromInt (raised // exp)
            else
                sign
                    ++ String.fromInt (raised // exp)
                    ++ "."
                    ++ String.fromInt decimals
