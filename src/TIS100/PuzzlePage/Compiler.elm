module TIS100.PuzzlePage.Compiler exposing (..)

import Html
import Parser.Advanced as Parser exposing (..)
import Set exposing (Set)


type alias Parser x =
    Parser.Parser () String x


main =
    let
        _ =
            run
                stmt
                "a:mov"
                |> Debug.log "Debug: "
    in
    Html.text ""


type Expr
    = OnlyLabel String
    | OnlyInst Inst
    | LabelInst String Inst


stmt : Parser Expr
stmt =
    oneOf
        [ labelPrefix
            |> andThen
                (\l ->
                    oneOf
                        [ succeed (LabelInst l)
                            |= inst
                        , succeed (OnlyLabel l)
                        ]
                )
        , succeed OnlyInst
            |= inst
        ]
        |. spaces
        |. end "Expecting end of line"


inst : Parser Inst
inst =
    oneOf
        [ succeed IMov
            |. keyword (Token "mov" "Invalid Op")
        , succeed INop
            |. keyword (Token "nop" "Invalid Op")
        ]
        |. spaceChars


spaceChars : Parser ()
spaceChars =
    Parser.chompWhile (\c -> c == ' ')


type Inst
    = IMov
    | INop



--type Op
--    = Nop
--    | Mov
--    | Add
--    | Sub
--    | Sav
--    | Jmp
--    | Jez
--    | Jgz
--    | Jlz
--    | Jro
--
--opParser : Parser Op
--opParser =
--    oneOf
--        [ succeed Nop |. keyword "nop"
--        , succeed Mov |. keyword "mov"
--        , succeed Add |. keyword "add"
--        , succeed Sub |. keyword "sub"
--        , succeed Sav |. keyword "sav"
--        , succeed Jmp |. keyword "jmp"
--        , succeed Jez |. keyword "jez"
--        , succeed Jgz |. keyword "jgz"
--        , succeed Jlz |. keyword "jlz"
--        , succeed Jro |. keyword "jro"
--        ]
--        |. spaceChars
--
--num : Parser LabelOrOp
--num =
--    Parser.succeed (Num.fromInt >> Num)
--        |= Parser.oneOf
--            [ Parser.succeed negate
--                |. Parser.symbol "-"
--                |= Parser.int
--            , Parser.int
--            ]


labelPrefix : Parser String
labelPrefix =
    Parser.variable
        { start = Char.isAlpha
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = opNames
        , expecting = "Invalid Op"
        }
        |. spaceChars
        |. symbol (Token ":" "Invalid Op")
        |. spaceChars


opNames : Set String
opNames =
    Set.fromList
        [ "nop"
        , "mov"
        , "add"
        , "sub"
        , "sav"
        , "jmp"
        , "jez"
        , "jgz"
        , "jlz"
        , "jro"
        ]
