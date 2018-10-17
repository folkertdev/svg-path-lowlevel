module Syntax exposing (coordinatePair, digitSequence, drawtos, expect, fuzzers, number, suite, tester, whitespaceParsing)

import Expect
import Fuzz exposing (bool, float, tuple, tuple3)
import Parser.Advanced as Parser
import Svg.Path.Syntax as Syntax exposing (DrawTo(..), Mode(..), MoveTo(..))
import Test exposing (Test, describe, test)


digitSequence =
    tester
        ( "digitSequence"
        , [ expect Syntax.digitSequence "000" (Ok 0)
          , expect Syntax.digitSequence "42" (Ok 42)
          , expect Syntax.digitSequence "042" (Ok 42)
          , expect Syntax.digitSequence "" (Err [ { col = 1, contextStack = [], problem = Syntax.ExpectingDigit, row = 1 } ])
          ]
        )


coordinatePair =
    tester
        ( "coordinatePair"
        , [ expect Syntax.coordinatePair "100-200" (Ok ( 100, -200 ))
          , expect Syntax.coordinatePair "0.6.5" (Ok ( 0.6, 0.5 ))
          , expect Syntax.coordinatePair "67.5-9.5" (Ok ( 67.5, -9.5 ))
          ]
        )


twoCoordinatePairs =
    tester
        ( "twoCoordinatePairs"
        , [ expect Syntax.twoCoordinatePairs "1,1,1,1" (Ok ( ( 1, 1 ), ( 1, 1 ) ))
          ]
        )


fuzzCoordinatePair =
    tuple3 ( float, bool, float )
        |> Fuzz.map
            (\( x, useComma, y ) ->
                let
                    separator =
                        if useComma then
                            ","

                        else
                            " "
                in
                ( ( x, y ), String.fromFloat x ++ separator ++ String.fromFloat y )
            )


fuzzTwoCoordinatePairs =
    tuple3 ( fuzzCoordinatePair, bool, fuzzCoordinatePair )
        |> Fuzz.map
            (\( ( p1, s1 ), useComma, ( p2, s2 ) ) ->
                let
                    separator =
                        if useComma then
                            ","

                        else
                            " "
                in
                ( ( p1, p2 ), s1 ++ separator ++ s2 )
            )


fuzzers =
    Test.describe "fuzzers"
        [ Test.fuzz float "number" <|
            \value ->
                case Parser.run Syntax.number (String.fromFloat value) of
                    Ok result ->
                        result |> Expect.within (Expect.Absolute 1.0e-6) value

                    Err e ->
                        Err e |> Expect.equal (Ok value)
        , Test.fuzz fuzzCoordinatePair "fuzzCoordinatePair" <|
            \( expected, string ) ->
                Parser.run Syntax.coordinatePair string
                    |> Expect.equal (Ok expected)
        , Test.fuzz fuzzTwoCoordinatePairs "twoCoordinatePairsFuzz" <|
            \( expected, string ) ->
                Parser.run Syntax.twoCoordinatePairs string
                    |> Expect.equal (Ok expected)
        ]


number =
    tester
        ( "number"
        , [ expect Syntax.number "100" (Ok 100)
          , expect Syntax.number "-200" (Ok -200)
          , expect Syntax.number "0.6" (Ok 0.6)
          , expect Syntax.number ".5" (Ok 0.5)
          , expect Syntax.number "4e2" (Ok 4.0e2)
          , expect Syntax.number "4.0e2" (Ok 4.0e2)
          ]
        )


whitespaceParsing : Test
whitespaceParsing =
    let
        desired1 =
            Ok
                [ ( MoveTo Syntax.Absolute ( 600, 350 )
                  , [ LineTo Syntax.Relative [ ( 10, 10 ) ], LineTo Syntax.Relative [ ( 20, 20 ) ], Closepath ]
                  )
                ]

        desired2 =
            Ok
                [ ( MoveTo Syntax.Absolute ( 150, 50 )
                  , [ QuadraticTo Syntax.Absolute [ ( ( 100, 50 ), ( 200, 100 ) ) ] ]
                  )
                ]
    in
    tester
        ( "svg path parsing uses whitespace permissively"
        , [ expect Syntax.svgPath "M600,350l10,10l20,20Z" desired1
          , expect Syntax.svgPath "M600,350 l10,10 l20,20 Z" desired1
          , expect Syntax.svgPath "M 600, 350 l 10, 10 l 20, 20 Z" desired1
          , expect Syntax.svgPath "M   600  ,  350 l  10  , 10 l 20  , 20  Z" desired1
          , expect Syntax.svgPath "M150,50Q100,50,200,100" desired2
          , expect Syntax.svgPath "M150,50Q100,50 200,100" desired2
          ]
        )


drawtos =
    let
        ellipticalArcExample =
            { radii = ( 25, 25 )
            , xAxisRotation = -30
            , largeArcFlag = False
            , sweepFlag = True
            , endpoint = ( 50, -25 )
            }
    in
    tester
        ( "drawtos"
        , [ expect Syntax.curveto "C 110.6,4.9,67.5-9.5,36.9,6.7" (Ok <| CubicTo Syntax.Absolute [ ( ( 110.6, 4.9 ), ( 67.5, -9.5 ), ( 36.9, 6.7 ) ) ])
          , expect Syntax.curveto "c 110.6,4.9,67.5-9.5,36.9,6.7" (Ok <| CubicTo Syntax.Relative [ ( ( 110.6, 4.9 ), ( 67.5, -9.5 ), ( 36.9, 6.7 ) ) ])
          , expect Syntax.moveto "M0,0" (Ok ( MoveTo Absolute ( 0, 0 ), Nothing ))
          , expect Syntax.moveto "M0.3,0" (Ok ( MoveTo Absolute ( 0.3, 0 ), Nothing ))
          , expect Syntax.moveto "m0,0" (Ok ( MoveTo Relative ( 0, 0 ), Nothing ))
          , expect Syntax.moveto "M0,0 20,20" (Ok ( MoveTo Absolute ( 0, 0 ), Just (LineTo Absolute [ ( 20, 20 ) ]) ))
          , expect Syntax.lineto "L0,0" (Ok <| LineTo Absolute [ ( 0, 0 ) ])
          , expect Syntax.lineto "l0,0" (Ok <| LineTo Relative [ ( 0, 0 ) ])
          , expect Syntax.horizontalLineto "H0 1 2 3" (Ok <| HorizontalTo Absolute [ 0, 1, 2, 3 ])
          , expect Syntax.horizontalLineto "h0 1 2 3" (Ok <| HorizontalTo Relative [ 0, 1, 2, 3 ])
          , expect Syntax.verticalLineto "V0 1 2 3" (Ok <| VerticalTo Absolute [ 0, 1, 2, 3 ])
          , expect Syntax.verticalLineto "v0 1 2 3" (Ok <| VerticalTo Relative [ 0, 1, 2, 3 ])
          , expect Syntax.closepath "Z" (Ok Closepath)
          , expect Syntax.closepath "z" (Ok Closepath)
          , expect Syntax.curveto "C100,100 250,100 250,200" (Ok <| CubicTo Absolute [ ( ( 100, 100 ), ( 250, 100 ), ( 250, 200 ) ) ])
          , expect Syntax.curveto "c100,100 250,100 250,200" (Ok <| CubicTo Relative [ ( ( 100, 100 ), ( 250, 100 ), ( 250, 200 ) ) ])
          , expect Syntax.smoothCurveto "S400,300 400,200" (Ok <| SmoothCubicTo Absolute [ ( ( 400, 300 ), ( 400, 200 ) ) ])
          , expect Syntax.smoothCurveto "s400,300 400,200" (Ok <| SmoothCubicTo Relative [ ( ( 400, 300 ), ( 400, 200 ) ) ])
          , expect Syntax.quadratic "Q400,50 600,300" (Ok <| QuadraticTo Absolute [ ( ( 400, 50 ), ( 600, 300 ) ) ])
          , expect Syntax.quadratic "q400,50 600,300" (Ok <| QuadraticTo Relative [ ( ( 400, 50 ), ( 600, 300 ) ) ])
          , expect Syntax.smoothQuadratic "T1000,300" (Ok <| SmoothQuadraticTo Absolute [ ( 1000, 300 ) ])
          , expect Syntax.smoothQuadratic "t1000,300" (Ok <| SmoothQuadraticTo Relative [ ( 1000, 300 ) ])
          , expect Syntax.ellipticalArc "A25,25 -30 0,1 50,-25" (Ok <| EllipticalArcTo Absolute [ ellipticalArcExample ])
          , expect Syntax.ellipticalArc "a25,25 -30 0,1 50,-25" (Ok <| EllipticalArcTo Relative [ ellipticalArcExample ])
          ]
        )


suite : Test
suite =
    describe "svg path syntax parser in elm" <|
        let
            serious =
                "M600,350 l10,10 l20,20 Z"

            curves =
                """M 213.1,6.7 
          C 110.6,4.9,67.5-9.5,36.9,6.7
          c -32.4-14.4-73.7,0-88.1,30.6
          C 110.6,4.9,67.5-9.5,36.9,6.7
          C 2.8,22.9-13.4,62.4,13.5,110.9
          C 33.3,145.1,67.5,170.3,125,217
          c 59.3-46.7,93.5-71.9,111.5-106.1
          C 263.4,64.2,247.2,22.9,213.1,6.7
                z """
        in
        [ test "moveto drawto command group" <|
            \_ ->
                Parser.run Syntax.movetoDrawtoCommandGroup serious
                    |> Expect.equal
                        (Ok
                            ( MoveTo Absolute ( 600, 350 )
                            , [ LineTo Relative [ ( 10, 10 ) ], LineTo Relative [ ( 20, 20 ) ], Closepath ]
                            )
                        )
        , test "moveto drawto command groups lines" <|
            \_ ->
                Parser.run Syntax.svgPath serious
                    |> Expect.equal
                        (Ok
                            [ ( MoveTo Absolute ( 600, 350 )
                              , [ LineTo Relative [ ( 10, 10 ) ], LineTo Relative [ ( 20, 20 ) ], Closepath ]
                              )
                            ]
                        )
        , test "relative moveto 0,0" <|
            \_ ->
                Parser.run Syntax.moveto "m 0,0"
                    |> Expect.equal (Ok ( MoveTo Relative ( 0, 0 ), Nothing ))
        , test "close path" <|
            \_ ->
                Parser.run Syntax.closepath "z"
                    |> Expect.equal (Ok Closepath)
        , test "path with only close path" <|
            \_ ->
                Parser.run Syntax.svgPath "M0,0\nz"
                    |> Expect.equal (Ok [ ( MoveTo Absolute ( 0, 0 ), [ Closepath ] ) ])
        , test "lineto command with multiple arguments " <|
            \_ ->
                Parser.run Syntax.lineto "l 10,10 20,20"
                    |> Expect.equal
                        (Ok (LineTo Relative [ ( 10, 10 ), ( 20, 20 ) ]))
        , test "moveto drawto command groups curves" <|
            \_ ->
                Parser.run Syntax.svgPath curves
                    |> Expect.equal
                        (Ok
                            [ ( MoveTo Absolute ( 213.1, 6.7 )
                              , [ CubicTo Absolute [ ( ( 110.6, 4.9 ), ( 67.5, -9.5 ), ( 36.9, 6.7 ) ) ]
                                , CubicTo Relative [ ( ( -32.4, -14.4 ), ( -73.7, 0 ), ( -88.1, 30.6 ) ) ]
                                , CubicTo Absolute [ ( ( 110.6, 4.9 ), ( 67.5, -9.5 ), ( 36.9, 6.7 ) ) ]
                                , CubicTo Absolute [ ( ( 2.8, 22.9 ), ( -13.4, 62.4 ), ( 13.5, 110.9 ) ) ]
                                , CubicTo Absolute [ ( ( 33.3, 145.1 ), ( 67.5, 170.3 ), ( 125, 217 ) ) ]
                                , CubicTo Relative [ ( ( 59.3, -46.7 ), ( 93.5, -71.9 ), ( 111.5, -106.1 ) ) ]
                                , CubicTo Absolute [ ( ( 263.4, 64.2 ), ( 247.2, 22.9 ), ( 213.1, 6.7 ) ) ]
                                , Closepath
                                ]
                              )
                            ]
                        )
        ]


expect : Parser.Parser c e a -> String -> Result (List (Parser.DeadEnd c e)) a -> Int -> String -> Test
expect parser string expected i name =
    test (name ++ "_" ++ String.fromInt i ++ "_" ++ string) <|
        \_ ->
            Parser.run parser string
                |> Expect.equal expected


tester ( name, tests ) =
    Test.describe name (List.indexedMap (\i expectation -> expectation i name) tests)
