module ToString exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Path.LowLevel exposing (..)
import Test exposing (..)


p =
    ( 42, 24 )


p2 =
    ( 11, 13 )


p3 =
    ( 1, 2 )


ellipticalArcArgument =
    { radii = ( 5, 10 )
    , xAxisRotate = 2
    , arcFlag = LargestArc
    , direction = Clockwise
    , target = p
    }


absoluteSubPath =
    { moveto = MoveTo Absolute p
    , drawtos =
        [ LineTo Absolute [ p ]
        , Horizontal Absolute [ Tuple.first p ]
        , Vertical Absolute [ Tuple.second p ]
        , CurveTo Absolute [ ( p, p2, p3 ) ]
        , SmoothCurveTo Absolute [ ( p, p2 ) ]
        , QuadraticBezierCurveTo Absolute [ ( p, p2 ) ]
        , SmoothQuadraticBezierCurveTo Absolute [ p ]
        , EllipticalArc Absolute [ ellipticalArcArgument ]
        , ClosePath
        ]
    }


relativeSubPath =
    { moveto = MoveTo Relative p
    , drawtos =
        [ LineTo Relative [ p ]
        , Horizontal Relative [ Tuple.first p ]
        , Vertical Relative [ Tuple.second p ]
        , CurveTo Relative [ ( p, p2, p3 ) ]
        , SmoothCurveTo Relative [ ( p, p2 ) ]
        , QuadraticBezierCurveTo Relative [ ( p, p2 ) ]
        , SmoothQuadraticBezierCurveTo Relative [ p ]
        , EllipticalArc Relative [ ellipticalArcArgument ]
        , ClosePath
        ]
    }


suite : Test
suite =
    describe "toString tests"
        [ test "absolute path gives expected outpu " <|
            \_ ->
                let
                    expected =
                        "M42,24 L42,24 H42 V24 C42,24 11,13 1,2 S42,24 11,13 Q42,24 11,13 T42,24 A5,10 2 1 0 42,24 Z"
                in
                Path.LowLevel.toString [ absoluteSubPath ]
                    |> Expect.equal expected
        , test "relative path gives expected outpu " <|
            \_ ->
                let
                    expected =
                        "m42,24 l42,24 h42 v24 c42,24 11,13 1,2 s42,24 11,13 q42,24 11,13 t42,24 a5,10 2 1 0 42,24 Z"
                in
                Path.LowLevel.toString [ relativeSubPath ]
                    |> Expect.equal expected
        ]
