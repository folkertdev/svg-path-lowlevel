module Svg.Path.Syntax exposing (Problem(..), flag)

import Parser.Advanced as Parser exposing ((|.), (|=), Parser, Step(..), Token(..), chompIf, chompWhile, inContext, oneOf, symbol)


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


{-| MoveTo is always ab
-}
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


type Problem
    = NumberProblem String
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


moveto =
    inContext (Instruction "MoveTo") <|
        instruction 'm' coordinatePair



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
            |. oneOf [ chompIf isWhitespace ExpectingWhitespace, Parser.succeed () ]
            |= argumentParser
        , Parser.succeed (Done (List.reverse reverseArguments))
        ]



-- whitespace separated


whitespaceSeparated : Parser c e argument -> Parser c e (List argument)
whitespaceSeparated elementParser =
    elementParser
        |> Parser.andThen (\initial -> Parser.loop [ initial ] (whitespaceSeparatedHelp elementParser))


whitespaceSeparatedHelp : Parser c e element -> List element -> Parser c e (Step (List element) (List element))
whitespaceSeparatedHelp elementParser reverseElements =
    oneOf
        [ Parser.succeed (\argument -> Loop (argument :: reverseElements))
            |. chompWhile isWhitespace
            |= elementParser
        , Parser.succeed (Done (List.reverse reverseElements))
        ]



-- coordinate pair


coordinatePair : Parser c Problem ( Float, Float )
coordinatePair =
    Parser.succeed Tuple.pair
        |= coordinate
        |. optional commaWsp
        |= coordinate



-- coordinate


coordinate : Parser c Problem Float
coordinate =
    number



-- non-negative number


nonNegativeNumber : Parser c Problem Float
nonNegativeNumber =
    oneOf
        [ floatingPointConstant
        , Parser.map toFloat integerConstant
        ]



-- number


number : Parser c Problem Float
number =
    oneOf
        [ Parser.succeed applySign
            |= sign
            |= nonNegativeNumber
        , nonNegativeNumber
        ]



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



-- floating point constant


floatingPointConstant : Parser c Problem Float
floatingPointConstant =
    oneOf
        [ fractionalConstant
        , fractionalConstantWithExponent
        , Parser.succeed (\base parsedExponent -> toFloat base * (10 ^ toFloat parsedExponent))
            |= digitSequence
            |= exponent
        ]



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
                toFloat lhs + floatRhs / toFloat shift
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
                    Parser.problem (NumberProblem "String.toInt returned Nothing")
    in
    (chompIf Char.isDigit ExpectingDigit |. chompWhile Char.isDigit)
        |> Parser.getChompedString
        |> Parser.andThen convertToInt
