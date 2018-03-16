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


noisyCurve : SubPath
noisyCurve =
    { moveto = MoveTo Absolute ( 0, 0 )
    , drawtos =
        [ CurveTo Absolute
            [ ( ( 50, 300 ), ( 115.48220313557538, 184.51779686442455 ), ( 150, 150 ) )
            , ( ( 174.4077682344544, 125.59223176554563 ), ( 201.50281615652946, 107.1143748231127 ), ( 225, 100 ) )
            , ( ( 242.50027908358396, 94.70134183997996 ), ( 259.3985449774631, 90.35777052211942 ), ( 275, 100 ) )
              {-
                 , ( ( 303.5728284700477, 117.6589791492101 ), ( 310.67715104458006, 225.6971428110722 ), ( 350, 250 ) )
                 , ( ( 387.1892544416877, 272.984223261231 ), ( 457.95062325807123, 267.41742213584274 ), ( 500, 250 ) )
                 , ( ( 540.8292528272556, 233.08796973739078 ), ( 561.9232752097317, 157.81554102840957 ), ( 600, 150 ) )
                 , ( ( 637.1510925306279, 142.37445212205927 ), ( 685.9957196353515, 182.48450132539352 ), ( 725, 200 ) )
                 , ( ( 760.5443248134045, 215.9617500525209 ), ( 793.6239497752703, 247.7701727655472 ), ( 825, 250 ) )
                 , ( ( 851.3840081587954, 251.87505372808639 ), ( 876.5829642941892, 219.4719877418889 ), ( 900, 225 ) )
                 , ( ( 927.1237694606573, 231.40305339874794 ), ( 975, 300 ), ( 975, 300 ) )
              -}
            ]
        ]
    }


suite : Test
suite =
    describe "toString tests"
        [ test "absolute path gives expected output " <|
            \_ ->
                let
                    expected =
                        "M42,24 L42,24 H42 V24 C42,24 11,13 1,2 S42,24 11,13 Q42,24 11,13 T42,24 A5,10 2 1 0 42,24 Z"
                in
                    Path.LowLevel.toString [ absoluteSubPath ]
                        |> Expect.equal expected
        , test "relative path gives expected output " <|
            \_ ->
                let
                    expected =
                        "m42,24 l42,24 h42 v24 c42,24 11,13 1,2 s42,24 11,13 q42,24 11,13 t42,24 a5,10 2 1 0 42,24 Z"
                in
                    Path.LowLevel.toString [ relativeSubPath ]
                        |> Expect.equal expected
        , test "toStringWith & decimalPlaces works as expected" <|
            \_ ->
                [ noisyCurve ]
                    |> Path.LowLevel.toStringWith [ decimalPlaces 3 ]
                    |> Expect.equal "M0,0 C50,300 115.482,184.518 150,150 174.408,125.592 201.503,107.114 225,100 242.500,94.701 259.399,90.358 275,100"
        ]
