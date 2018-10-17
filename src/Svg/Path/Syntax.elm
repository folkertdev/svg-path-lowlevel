module Svg.Path.Syntax exposing (ArcFlag, Context(..), DrawTo(..), EllipticalArcArgument, Mode(..), MoveTo(..), Problem(..), Sign(..), SweepFlag, applyConstructor, applySign, argumentSequence, argumentSequenceHelp, closepath, commaWsp, coordinate, coordinatePair, curveto, digitSequence, drawtoCommand, drawtoCommands, ellipticalArc, ellipticalArcArgument, exponent, flag, fractionalConstant, fractionalConstantWithExponent, horizontalLineto, instruction, integerConstant, isWhitespace, lineto, mode, moveto, movetoDrawtoCommandGroup, movetoDrawtoCommandGroups, number, optional, quadratic, sign, signedDigitSequence, smoothCurveto, smoothQuadratic, svgPath, threeCoordinatePairs, twoCoordinatePairs, verticalLineto, whitespaceSeparated, whitespaceSeparatedHelp)

import Parser.Advanced as Parser exposing ((|.), (|=), Parser, Step(..), Token(..), backtrackable, chompIf, chompWhile, inContext, oneOf, symbol)


svgPath =
    Parser.succeed identity
        |. chompWhile isWhitespace
        |= oneOf [ movetoDrawtoCommandGroups, Parser.succeed [] ]
        |. chompWhile isWhitespace


movetoDrawtoCommandGroups =
    whitespaceSeparated movetoDrawtoCommandGroup


movetoDrawtoCommandGroup =
    Parser.succeed Tuple.pair
        |= moveto
        |. chompWhile isWhitespace
        |= oneOf [ drawtoCommands, Parser.succeed [] ]
        |> Parser.map
            (\( ( move, maybeImplicitLineTo ), drawtos ) ->
                case maybeImplicitLineTo of
                    Just implicitLineTo ->
                        ( move, implicitLineTo :: drawtos )

                    Nothing ->
                        ( move, drawtos )
            )


drawtoCommands =
    whitespaceSeparated drawtoCommand


drawtoCommand =
    oneOf
        [ closepath
        , lineto
        , horizontalLineto
        , verticalLineto
        , curveto
        , smoothCurveto
        , quadratic
        , smoothQuadratic
        , ellipticalArc
        ]


{-| -}
type MoveTo
    = MoveTo Mode ( Float, Float )


type DrawTo
    = LineTo Mode (List ( Float, Float ))
    | HorizontalTo Mode (List Float)
    | VerticalTo Mode (List Float)
    | CubicTo Mode (List ( ( Float, Float ), ( Float, Float ), ( Float, Float ) ))
    | SmoothCubicTo Mode (List ( ( Float, Float ), ( Float, Float ) ))
    | QuadraticTo Mode (List ( ( Float, Float ), ( Float, Float ) ))
    | SmoothQuadraticTo Mode (List ( Float, Float ))
    | EllipticalArcTo Mode (List EllipticalArcArgument)
    | Closepath


mode : DrawTo -> Mode
mode drawto =
    case drawto of
        LineTo drawtoMode _ ->
            drawtoMode

        HorizontalTo drawtoMode _ ->
            drawtoMode

        VerticalTo drawtoMode _ ->
            drawtoMode

        CubicTo drawtoMode _ ->
            drawtoMode

        SmoothCubicTo drawtoMode _ ->
            drawtoMode

        QuadraticTo drawtoMode _ ->
            drawtoMode

        SmoothQuadraticTo drawtoMode _ ->
            drawtoMode

        EllipticalArcTo drawtoMode _ ->
            drawtoMode

        Closepath ->
            Absolute


type alias EllipticalArcArgument =
    { radii : ( Float, Float )
    , xAxisRotation : Float
    , largeArcFlag : ArcFlag
    , sweepFlag : SweepFlag
    , endpoint : ( Float, Float )
    }


type alias ArcFlag =
    Bool


type alias SweepFlag =
    Bool


applyConstructor : (Mode -> argument -> instruction) -> ( Mode, argument ) -> instruction
applyConstructor constructor ( argumentMode, argument ) =
    constructor argumentMode argument


type Context
    = Instruction String
    | Type String


type Problem
    = NumberProblem String
    | NeverOccurs
    | ExpectingFlag
    | ExpectingInstruction Char
    | ExpectingSign
    | ExpectingCharacter Char
    | ExpectingWhitespace
    | ExpectingDot
    | ExpectingExponent
    | ExpectingDigit


isWhitespace : Char -> Bool
isWhitespace c =
    c == '\t' || c == ' ' || c == '\n' || c == '\u{000C}' || c == '\u{000D}'


commaWsp : Parser c Problem ()
commaWsp =
    let
        withLeadingWhitespace =
            chompIf isWhitespace ExpectingWhitespace
                |. chompWhile isWhitespace
                |. oneOf [ chompIf (\c -> c == ',') (ExpectingCharacter ','), Parser.succeed () ]
                |. chompWhile isWhitespace

        withLeadingComma =
            chompIf (\c -> c == ',') (ExpectingCharacter ',')
                |. chompWhile isWhitespace
    in
    oneOf
        [ withLeadingWhitespace
        , withLeadingComma
        ]


optional : Parser c e a -> Parser c e ()
optional parser =
    oneOf [ Parser.map (\_ -> ()) parser, Parser.succeed () ]



-- moveto


moveto : Parser Context Problem ( MoveTo, Maybe DrawTo )
moveto =
    instruction 'm' coordinatePair
        |> Parser.andThen
            (\( parsedMode, pairs ) ->
                case pairs of
                    [] ->
                        Parser.problem NeverOccurs

                    [ x ] ->
                        Parser.succeed ( MoveTo parsedMode x, Nothing )

                    x :: xs ->
                        Parser.succeed ( MoveTo parsedMode x, Just (LineTo parsedMode xs) )
            )
        |> inContext (Instruction "MoveTo")



-- closepath


closepath =
    inContext (Instruction "Close Path") <|
        Parser.map (\_ -> Closepath) (chompIf (\c -> c == 'z' || c == 'Z') (ExpectingInstruction 'z'))



-- lineto


lineto =
    inContext (Instruction "LineTo") <|
        Parser.map (applyConstructor LineTo) (instruction 'l' coordinatePair)


horizontalLineto =
    inContext (Instruction "Horizontal LineTo") <|
        Parser.map (applyConstructor HorizontalTo) (instruction 'h' coordinate)


verticalLineto =
    inContext (Instruction "Vertical LineTo") <|
        Parser.map (applyConstructor VerticalTo) (instruction 'v' coordinate)



-- curveto


curveto =
    inContext (Instruction "Cubic CurveTo") <|
        Parser.map (applyConstructor CubicTo) (instruction 'c' threeCoordinatePairs)


smoothCurveto =
    inContext (Instruction "Smooth Cubic CurveTo") <|
        Parser.map (applyConstructor SmoothCubicTo) (instruction 's' twoCoordinatePairs)



-- quadratic curveto


quadratic =
    inContext (Instruction "Quadratic CurveTo") <|
        Parser.map (applyConstructor QuadraticTo) (instruction 'q' twoCoordinatePairs)


smoothQuadratic =
    inContext (Instruction "Smooth Quadratic CurveTo") <|
        Parser.map (applyConstructor SmoothQuadraticTo) (instruction 't' coordinatePair)



-- elliptical arc


ellipticalArc =
    inContext (Instruction "Elliptical Arc") <|
        Parser.map (applyConstructor EllipticalArcTo) (instruction 'a' ellipticalArcArgument)


ellipticalArcArgument =
    let
        constructor rx ry xAxisRotation largeArcFlag sweepFlag endpoint =
            { radii = ( rx, ry )
            , xAxisRotation = xAxisRotation
            , largeArcFlag = largeArcFlag
            , sweepFlag = sweepFlag
            , endpoint = endpoint
            }
    in
    Parser.succeed constructor
        |= number
        |. optional commaWsp
        |= number
        |. optional commaWsp
        |= number
        |. commaWsp
        |= flag
        |. optional commaWsp
        |= flag
        |. optional commaWsp
        |= coordinatePair



-- coordinate pair helpers


twoCoordinatePairs =
    Parser.succeed Tuple.pair
        |= coordinatePair
        |. optional commaWsp
        |= coordinatePair
        |. optional commaWsp


threeCoordinatePairs =
    Parser.succeed (\x y z -> ( x, y, z ))
        |= coordinatePair
        |. optional commaWsp
        |= coordinatePair
        |. optional commaWsp
        |= coordinatePair
        |. optional commaWsp



-- instruction


type Mode
    = Absolute
    | Relative


instruction : Char -> Parser c Problem argument -> Parser c Problem ( Mode, List argument )
instruction char argumentParser =
    oneOf
        [ Parser.succeed (\arguments -> ( Relative, arguments ))
            |. chompIf (\c -> c == Char.toLower char) (ExpectingInstruction char)
            |. chompWhile isWhitespace
            |= argumentSequence argumentParser
        , Parser.succeed (\arguments -> ( Absolute, arguments ))
            |. chompIf (\c -> c == Char.toUpper char) (ExpectingInstruction char)
            |. chompWhile isWhitespace
            |= argumentSequence argumentParser
        ]



-- argument sequence


argumentSequence : Parser c Problem argument -> Parser c Problem (List argument)
argumentSequence argumentParser =
    argumentParser
        |> Parser.andThen (\initial -> Parser.loop [ initial ] (argumentSequenceHelp argumentParser))


argumentSequenceHelp : Parser c Problem argument -> List argument -> Parser c Problem (Step (List argument) (List argument))
argumentSequenceHelp argumentParser reverseArguments =
    oneOf
        [ Parser.succeed (\argument -> Loop (argument :: reverseArguments))
            -- if we parse whitespace, but there is no argument, then we're done and should still succeed
            |. oneOf [ backtrackable (chompIf isWhitespace ExpectingWhitespace), Parser.succeed () ]
            |= argumentParser
        , Parser.succeed (Done (List.reverse reverseArguments))
        ]



-- whitespace separated


whitespaceSeparated : Parser c e argument -> Parser c e (List argument)
whitespaceSeparated elementParser =
    Parser.loop [] (whitespaceSeparatedHelp elementParser)


whitespaceSeparatedHelp : Parser c e element -> List element -> Parser c e (Step (List element) (List element))
whitespaceSeparatedHelp elementParser reverseElements =
    oneOf
        [ Parser.succeed (\argument -> Loop (argument :: reverseElements))
            |. backtrackable (chompWhile isWhitespace)
            |= elementParser
        , Parser.succeed (Done (List.reverse reverseElements))
        ]



-- coordinate pair


coordinatePair : Parser Context Problem ( Float, Float )
coordinatePair =
    Parser.succeed Tuple.pair
        |= coordinate
        |. optional commaWsp
        |= coordinate
        |> Parser.inContext (Type "coordinatePair")



-- coordinate


coordinate : Parser c Problem Float
coordinate =
    number



-- number


number : Parser c Problem Float
number =
    Parser.succeed
        (\parsedSign value ->
            case parsedSign of
                Positive ->
                    value

                Negative ->
                    -value
        )
        |= oneOf [ backtrackable sign, Parser.succeed Positive ]
        |= Parser.float (NumberProblem "ExpectedFloat") (NumberProblem "InvalidNumber")



-- flag


flag : Parser c Problem Bool
flag =
    oneOf
        [ symbol (Token "1" ExpectingFlag)
            |> Parser.map (\_ -> True)
        , symbol (Token "0" ExpectingFlag)
            |> Parser.map (\_ -> False)
        ]



-- integer constant


integerConstant : Parser c Problem Int
integerConstant =
    digitSequence



-- fractional constant


fractionalConstant : Parser c Problem Float
fractionalConstant =
    let
        dot =
            chompIf (\c -> c == '.') ExpectingDot

        parts : Parser c Problem ( Int, Int )
        parts =
            oneOf
                [ Parser.succeed Tuple.pair
                    |= digitSequence
                    |. dot
                    |= digitSequence
                , Parser.succeed (\rhs -> ( 0, rhs ))
                    |. dot
                    |= digitSequence
                , Parser.succeed (\lhs -> ( lhs, 0 ))
                    |= digitSequence
                    |. dot
                ]

        asFloat : ( Int, Int ) -> Float
        asFloat ( lhs, rhs ) =
            let
                floatRhs =
                    toFloat rhs

                shift =
                    ceiling (logBase 10 floatRhs)
            in
            if rhs == 0 then
                toFloat lhs

            else
                toFloat lhs + floatRhs / toFloat (10 ^ shift)
    in
    Parser.map asFloat parts


fractionalConstantWithExponent : Parser c Problem Float
fractionalConstantWithExponent =
    Parser.succeed (\base parsedExponent -> base * (10 ^ toFloat parsedExponent))
        |= fractionalConstant
        |= exponent



-- exponent


exponent : Parser c Problem Int
exponent =
    Parser.succeed identity
        |. chompIf (\c -> c == 'e' || c == 'E') ExpectingExponent
        |= signedDigitSequence



-- sign


type Sign
    = Positive
    | Negative


sign : Parser c Problem Sign
sign =
    oneOf
        [ chompIf (\c -> c == '+') ExpectingSign
            |> Parser.map (\_ -> Positive)
        , chompIf (\c -> c == '-') ExpectingSign
            |> Parser.map (\_ -> Negative)
        ]


applySign : Sign -> number -> number
applySign parsedSign parsedNumber =
    case parsedSign of
        Positive ->
            parsedNumber

        Negative ->
            -parsedNumber


signedDigitSequence : Parser c Problem Int
signedDigitSequence =
    oneOf
        [ Parser.succeed applySign
            |= sign
            |= digitSequence
        , digitSequence
        ]



-- digit sequence


digitSequence : Parser c Problem Int
digitSequence =
    let
        convertToInt digitString =
            case String.toInt digitString of
                Just int ->
                    Parser.succeed int

                Nothing ->
                    Parser.problem
                        (NumberProblem <| "String.toInt returned Nothing for `" ++ digitString ++ "`")
    in
    (chompIf Char.isDigit ExpectingDigit |. chompWhile Char.isDigit)
        |> Parser.getChompedString
        |> Parser.andThen convertToInt
