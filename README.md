# SVG Path LowLevel 

A package for working with svg path syntax. This is a lowlevel package that 
provides freedom but little safety or convenience. It can 

* Convert svg path strings into an elm data type
* Convert elm into svg path strings

## Parsing svg path strings

This uses the [elm-tools/parser] package to parse the string and give nice error messages.
The result (if no parse errors occur) is a list of subpaths, the core building block consisting of one move instruction and 
a (possibly empty) list of drawto instructions (like lineTo or quadraticCurveTo).

```elm
import Path.LowLevel.Parser as PathParser

parsed : Result (List Parser.DeadEnd) (List SubPath) 
parsed = 
    PathParser.parse "M0,0 L10,10 10,15"
```

## Pretty-printing svg path strings

This package can turn subpaths back into a string, with some options to make the output string a little smaller.

```elm
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
```

## Data structures 


*I highly recommend you read [the MDN article on svg paths](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths). 
For some of the technical details, [the spec](https://www.w3.org/TR/SVG/paths.html) is also helpful.*


The core abstraction is a `SubPath`, a moveto and a list of drawto instructions. 

```elm
{-| A subpath is a `MoveTo` followed by a list of `DrawTo`s
-}
type alias SubPath =
    { moveto : MoveTo, drawtos : List DrawTo }
```

The instructions are custom types 

```elm
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
```


