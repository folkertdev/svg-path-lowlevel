module Path.LowLevel
    exposing
        ( ArcFlag(..)
        , Coordinate
        , Direction(..)
        , DrawTo(..)
        , EllipticalArcArgument
        , Mode(..)
        , MoveTo(..)
        , Precision(..)
        , SubPath
        , decodeFlags
        , encodeFlags
        , toString
        )

{-| A low-level package for working with svg path strings

This package has two use cases

  - **convert Elm data into svg paths**
  - **convert svg path syntax into elm data**

It is meant as a foundation: there is little type safety and convenience.
It's just a literal translation of the SVG spec into elm data types.

@docs toString, Precision
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


{-| Customize number of decimals in the output string

The string representation of a float can be quite long, and for most purposes a couple of decimals gives
enough precision. Limit the number of decimals with `AtMost`, or get the full string with `FullPrecision`.

-}
type Precision
    = FullPrecision
    | AtMost Int


{-| Convert a list of subpaths to svg path syntax

    let
        myPath =
            [ { moveto = MoveTo Relative ( 10, 20 )
              , drawtos =
                    [ EllipticalArc Absolute
                        { radii = ( 25, 25 )
                        , xAxisRotate = -30
                        , arcFlag = SmallestArc
                        , direction = CounterClockwise
                        , target = ( 50, -25 )
                        }
                    ]
              }
            ]
    in
        toString myPath --> "m10,20 A25,25 -30 0,1 50,-25"

-}
toString : Precision -> List SubPath -> String
toString precision subpaths =
    subpaths
        |> List.map (toStringSubPath precision)
        |> String.join " "


precisionFormatter : Precision -> (Float -> String)
precisionFormatter p =
    case p of
        FullPrecision ->
            floatFullPrecision

        AtMost n ->
            floatWithDecimals n


toStringSubPath : Precision -> SubPath -> String
toStringSubPath precision { moveto, drawtos } =
    stringifyMoveTo precision moveto ++ " " ++ String.join " " (List.map (stringifyDrawTo precision) drawtos)


stringifyMoveTo : Precision -> MoveTo -> String
stringifyMoveTo precision (MoveTo mode coordinate) =
    case mode of
        Absolute ->
            "M" ++ stringifyCoordinate precision coordinate

        Relative ->
            "m" ++ stringifyCoordinate precision coordinate


stringifyDrawTo : Precision -> DrawTo -> String
stringifyDrawTo precision command =
    if isEmpty command then
        ""
    else
        case command of
            LineTo mode coordinates ->
                stringifyCharacter mode 'L' ++ String.join " " (List.map (stringifyCoordinate precision) coordinates)

            Horizontal mode coordinates ->
                if List.isEmpty coordinates then
                    ""
                else
                    stringifyCharacter mode 'H' ++ String.join " " (List.map Basics.toString coordinates)

            Vertical mode coordinates ->
                stringifyCharacter mode 'V' ++ String.join " " (List.map Basics.toString coordinates)

            CurveTo mode coordinates ->
                stringifyCharacter mode 'C' ++ String.join " " (List.map (stringifyCoordinate3 precision) coordinates)

            SmoothCurveTo mode coordinates ->
                stringifyCharacter mode 'S' ++ String.join " " (List.map (stringifyCoordinate2 precision) coordinates)

            QuadraticBezierCurveTo mode coordinates ->
                stringifyCharacter mode 'Q' ++ String.join " " (List.map (stringifyCoordinate2 precision) coordinates)

            SmoothQuadraticBezierCurveTo mode coordinates ->
                stringifyCharacter mode 'T' ++ String.join " " (List.map (stringifyCoordinate precision) coordinates)

            EllipticalArc mode arguments ->
                stringifyCharacter mode 'A' ++ String.join " " (List.map (stringifyEllipticalArcArgument precision) arguments)

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


stringifyEllipticalArcArgument : Precision -> EllipticalArcArgument -> String
stringifyEllipticalArcArgument precision { radii, xAxisRotate, arcFlag, direction, target } =
    let
        ( arc, sweep ) =
            encodeFlags ( arcFlag, direction )
    in
    String.join " "
        [ stringifyCoordinate precision radii
        , Basics.toString xAxisRotate
        , Basics.toString arc
        , Basics.toString sweep
        , stringifyCoordinate precision target
        ]


stringifyCharacter : Mode -> Char -> String
stringifyCharacter mode character =
    case mode of
        Absolute ->
            String.fromChar (Char.toUpper character)

        Relative ->
            String.fromChar (Char.toLower character)


stringifyCoordinate : Precision -> Coordinate -> String
stringifyCoordinate precision ( x, y ) =
    let
        formatter =
            precisionFormatter precision
    in
    formatter x ++ "," ++ formatter y


stringifyCoordinate2 : Precision -> ( Coordinate, Coordinate ) -> String
stringifyCoordinate2 precision ( c1, c2 ) =
    stringifyCoordinate precision c1 ++ " " ++ stringifyCoordinate precision c2


stringifyCoordinate3 : Precision -> ( Coordinate, Coordinate, Coordinate ) -> String
stringifyCoordinate3 precision ( c1, c2, c3 ) =
    stringifyCoordinate precision c1 ++ " " ++ stringifyCoordinate precision c2 ++ " " ++ stringifyCoordinate precision c3


type alias FloatFormatter =
    Float -> String


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
        Basics.toString (round value)
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
                rem raised exp
        in
        if decimals == 0 then
            sign ++ Basics.toString (raised // exp)
        else
            sign
                ++ Basics.toString (raised // exp)
                ++ "."
                ++ Basics.toString decimals


floatFullPrecision : Float -> String
floatFullPrecision =
    Basics.toString
