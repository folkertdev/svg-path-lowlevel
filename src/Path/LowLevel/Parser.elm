module Path.LowLevel.Parser exposing (parse)

{-| Parse SVG path syntax into an elm value

@docs parse

-}

import Parser
import Path.LowLevel exposing (SubPath)
import Path.LowLevel.ParserInternal exposing (svgMixedPath)



-- actual implementation is in ParserInternal so the individual parsing functions can be tested


{-| Parse svg path syntax into a list of subpaths.

    parse "m10,20 A25,25 -30 0,1 50,-25"
        -->
        Ok
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

-}
parse : String -> Result (List Parser.DeadEnd) (List SubPath)
parse input =
    Parser.run svgMixedPath input
        |> Result.map (List.map (\( moveto, drawtos ) -> SubPath moveto drawtos))
