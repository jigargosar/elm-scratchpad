module TIS100.PuzzlePage.Compiler exposing
    ( Error(..)
    , ErrorDetail
    , Errors
    , compile
    , errorsToDetails
    , getErrorDetails
    , lexLine
    , prefixLabelToken
    , wordToken
    )

import List.Extra
import Parser exposing (..)
import Set exposing (Set)
import TIS100.Num as Num exposing (Num)
import TIS100.PuzzlePage.Inst exposing (..)
import Utils as U exposing (Dir4(..))


type Error
    = InvalidOpCode Int String
    | InvalidExpression Int String
    | MissingOperand Int Int
    | TooManyOperands Int Int
    | InternalError
    | UndefinedLabel Int String
    | LabelAlreadyDefined Int String


type alias ErrorDetail =
    { row : Int
    , startCol : Int
    , endCol : Int
    , msg : String
    }


type alias Errors =
    List ( Int, Error )


errorsToDetails : Errors -> List ErrorDetail
errorsToDetails =
    List.filterMap
        (\( l, e ) ->
            case e of
                InvalidOpCode startCol string ->
                    Just
                        { row = l
                        , startCol = startCol
                        , endCol = startCol + String.length string - 1
                        , msg = "Invalid opcode \"" ++ string ++ "\""
                        }

                InvalidExpression startCol string ->
                    Just
                        { row = l
                        , startCol = startCol
                        , endCol = startCol + String.length string - 1
                        , msg = "Invalid op:\"" ++ string ++ "\""
                        }

                UndefinedLabel startCol string ->
                    Just
                        { row = l
                        , startCol = startCol
                        , endCol = startCol + String.length string - 1
                        , msg = "undefined label"
                        }

                LabelAlreadyDefined startCol string ->
                    Just
                        { row = l
                        , startCol = startCol
                        , endCol = startCol + String.length string - 1
                        , msg = "label already defined"
                        }

                MissingOperand startCol endCol ->
                    Just
                        { row = l
                        , startCol = startCol
                        , endCol = endCol
                        , msg = "missing operand"
                        }

                TooManyOperands startCol endCol ->
                    Just
                        { row = l
                        , startCol = startCol
                        , endCol = endCol
                        , msg = "too many operands"
                        }

                InternalError ->
                    Nothing
        )


getErrorDetails : String -> List ErrorDetail
getErrorDetails srcCode =
    case compile srcCode of
        Err errors ->
            errorsToDetails errors

        Ok _ ->
            []


type alias PLine =
    { lineNo : Int
    , labels : Set String
    , inst : Inst
    }


type alias Prg =
    List PLine


compile : String -> Result Errors (List PLine)
compile string =
    string
        |> String.split "\n"
        |> List.indexedMap U.pair
        |> List.map (U.mapFirst U.inc)
        |> compileLines
        |> (\( es, os ) ->
                case es of
                    [] ->
                        Ok (toPLines os)

                    _ ->
                        Err es
           )
        |> Result.mapError (List.sortBy U.first)


toPLines : List ( Int, Stmt ) -> List PLine
toPLines =
    let
        insertMaybeLabel mbl =
            case mbl of
                Just (Label { val }) ->
                    Set.insert val

                _ ->
                    identity

        step ( row, stmt ) acc =
            case stmt of
                Stmt mbl (Just inst) ->
                    { acc
                        | revPLines =
                            PLine
                                row
                                (insertMaybeLabel mbl acc.prevLabels)
                                inst
                                :: acc.revPLines
                        , prevLabels = Set.empty
                    }

                Stmt mbl Nothing ->
                    { acc | prevLabels = insertMaybeLabel mbl acc.prevLabels }

        done acc =
            acc.revPLines
                |> List.reverse
                |> mapHead
                    (\pl ->
                        { pl | labels = Set.union acc.prevLabels pl.labels }
                    )
    in
    List.foldl step { prevLabels = Set.empty, revPLines = [] }
        >> done


mapHead : (a -> a) -> List a -> List a
mapHead fn xs =
    case xs of
        [] ->
            []

        h :: t ->
            fn h :: t


type alias PrefixLabels =
    Set String


type alias CAcc =
    { prevLabels : Set String
    , revStmts : List ( Int, Stmt )
    , revErrors : Errors
    }


compileLines : List ( Int, String ) -> ( Errors, List ( Int, Stmt ) )
compileLines ls =
    let
        allPrefixLabels : Set String
        allPrefixLabels =
            ls
                |> List.foldr
                    (\( _, line ) ->
                        case lexLine line of
                            Ok ((Token (PrefixLabel lbl) _) :: _) ->
                                Set.insert lbl

                            _ ->
                                identity
                    )
                    Set.empty

        done acc =
            ( acc.revErrors |> List.reverse, acc.revStmts |> List.reverse )
    in
    ls
        |> List.foldl (compileLinesHelp allPrefixLabels)
            { revErrors = []
            , revStmts = []
            , prevLabels = Set.empty
            }
        |> done


compileLinesHelp : Set String -> ( Int, String ) -> CAcc -> CAcc
compileLinesHelp allPrefixLabels ( row, line ) acc =
    let
        ( maybeLabel, res ) =
            case lexLine line of
                Ok tokens ->
                    case tokens of
                        (Token (PrefixLabel lbl) (Loc col _)) :: rest ->
                            ( Just lbl
                            , if Set.member lbl acc.prevLabels then
                                Err (LabelAlreadyDefined col lbl)

                              else
                                parseInst
                                    allPrefixLabels
                                    rest
                                    |> Result.map
                                        (stmtWithLabel
                                            (Label
                                                { col = col
                                                , val = lbl
                                                }
                                            )
                                        )
                            )

                        _ ->
                            ( Nothing
                            , parseInst
                                allPrefixLabels
                                tokens
                                |> Result.map stmtWithoutLabel
                            )

                Err _ ->
                    ( Nothing, Err InternalError )
    in
    case res of
        Ok stmt ->
            { acc
                | revStmts = ( row, stmt ) :: acc.revStmts
                , prevLabels = insertMaybe maybeLabel acc.prevLabels
            }

        Err err ->
            { acc
                | revErrors = ( row, err ) :: acc.revErrors
                , prevLabels = insertMaybe maybeLabel acc.prevLabels
            }


insertMaybe : Maybe comparable -> Set comparable -> Set comparable
insertMaybe mb =
    case mb of
        Just v ->
            Set.insert v

        Nothing ->
            identity


type Stmt
    = Stmt (Maybe Label) (Maybe Inst)


stmtWithLabel : Label -> Maybe Inst -> Stmt
stmtWithLabel label =
    Stmt (Just label)


stmtWithoutLabel : Maybe Inst -> Stmt
stmtWithoutLabel =
    Stmt Nothing


parseInst : PrefixLabels -> List Token -> Result Error (Maybe Inst)
parseInst prefixLabels tokens =
    case tokens of
        [] ->
            Ok Nothing

        x :: xs ->
            parseInstHelp prefixLabels x xs
                |> Result.map Just


parseInstHelp : PrefixLabels -> Token -> List Token -> Result Error Inst
parseInstHelp prefixLabels fst rest =
    case fst of
        Token (OpCode op) _ ->
            case op of
                NOP ->
                    withZeroArgOp Nop rest

                JMP ->
                    with1ArgOp (parseJumpInst prefixLabels Jmp) fst rest

                MOV ->
                    with2ArgOp parseMovInst fst rest

        _ ->
            invalidOp fst


parseJumpInst : PrefixLabels -> (Label -> value) -> Token -> Result Error value
parseJumpInst prefixLabels fn (Token _ (Loc col string)) =
    if Set.member string prefixLabels then
        Ok (fn (Label { col = col, val = string }))

    else
        Err (UndefinedLabel col string)


parseMovInst : Token -> Token -> Result Error Inst
parseMovInst a b =
    Result.map2 Mov
        (parseSrcOperand a)
        (parseDstOperand b)


parseDstOperand : Token -> Result Error Dst
parseDstOperand ((Token typ _) as t) =
    case typ of
        DIR dir ->
            Ok <| DstPort dir

        ACC ->
            Ok <| DstAcc

        NIL ->
            Ok DstNil

        _ ->
            invalidExpr t


parseSrcOperand : Token -> Result Error Src
parseSrcOperand ((Token typ _) as t) =
    case typ of
        DIR dir ->
            Ok <| SrcPort dir

        NUM num ->
            Ok <| SrcNum num

        ACC ->
            Ok <| SrcAcc

        _ ->
            invalidExpr t


withZeroArgOp : v -> List Token -> Result Error v
withZeroArgOp v rest =
    case rest of
        [] ->
            Ok v

        x :: xs ->
            tooManyOperands x xs


with1ArgOp :
    (Token -> Result Error value)
    -> Token
    -> List Token
    -> Result Error value
with1ArgOp fn fst rest =
    case rest of
        a :: [] ->
            fn a

        [] ->
            missingOperand fst fst

        _ :: x :: xs ->
            tooManyOperands x xs


with2ArgOp :
    (Token -> Token -> Result Error value)
    -> Token
    -> List Token
    -> Result Error value
with2ArgOp fn fst rest =
    case rest of
        a :: b :: [] ->
            --parseMoveInst a b
            fn a b

        [] ->
            missingOperand fst fst

        last :: [] ->
            missingOperand fst last

        _ :: _ :: x :: xs ->
            tooManyOperands x xs


tooManyOperands : Token -> List Token -> Result Error value
tooManyOperands first rest =
    let
        last =
            List.Extra.last rest
                |> Maybe.withDefault first
    in
    Err (TooManyOperands (tokenStartColumn first) (tokenEndColumn last))


missingOperand : Token -> Token -> Result Error value
missingOperand start end =
    Err (MissingOperand (tokenStartColumn start) (tokenEndColumn end))


invalidOp : Token -> Result Error value
invalidOp (Token _ (Loc col string)) =
    Err (InvalidOpCode col string)


invalidExpr : Token -> Result Error value
invalidExpr (Token _ (Loc col string)) =
    Err (InvalidExpression col string)


tokenStartColumn : Token -> Int
tokenStartColumn token =
    case token of
        Token _ (Loc col _) ->
            col


tokenEndColumn : Token -> Int
tokenEndColumn token =
    case token of
        Token _ (Loc col string) ->
            col + String.length string - 1


lexLine : String -> Result (List DeadEnd) (List Token)
lexLine src =
    Parser.run tokenListParser (String.toLower src)


type Token
    = Token TokenTyp Loc


type Loc
    = Loc Int String


type TokenTyp
    = Word String
    | PrefixLabel String
    | OpCode OpCode
    | DIR Dir4
    | ACC
    | NIL
    | NUM Num


type OpCode
    = NOP
    | MOV
    | JMP


wordToken : Int -> String -> Token
wordToken col string =
    Token (Word string) (Loc col string)


prefixLabelToken : Int -> String -> Token
prefixLabelToken col str =
    Token (PrefixLabel str) (Loc col str)


tokenListParser : Parser (List Token)
tokenListParser =
    succeed identity
        |. spaces
        |= oneOf
            [ backtrackable prefixLabelTokenParser
                |> andThen (\t -> loop [ t ] tokenListParserHelp)
            , loop [] tokenListParserHelp
            ]


tokenListParserHelp : List Token -> Parser (Step (List Token) (List Token))
tokenListParserHelp rs =
    succeed identity
        |. spaces
        |= oneOf
            [ succeed (Loop rs)
                |. lineComment "#"
            , succeed (\t -> Loop (t :: rs))
                |= tokenParser
            , succeed ()
                |. end
                |> map (\_ -> Done <| List.reverse rs)
            ]


tokenParser : Parser Token
tokenParser =
    oneOf
        [ opCodeTokenParser
        , toTokenParser (keyword2 ACC "acc")
        , toTokenParser (keyword2 NIL "nil")
        , numTokenParser
        , dirTokenParser
        , wordTokenParser
        ]


wordTokenParser : Parser Token
wordTokenParser =
    toTokenParser
        (succeed Word
            |= variable
                { start = isWordChar
                , inner = isWordChar
                , reserved = Set.empty
                }
        )


opCodeTokenParser : Parser Token
opCodeTokenParser =
    toTokenParser
        (succeed OpCode
            |= oneOf
                [ keyword2 MOV "mov"
                , keyword2 NOP "nop"
                , keyword2 JMP "jmp"
                ]
        )


dirTokenParser : Parser Token
dirTokenParser =
    toTokenParser
        (succeed DIR
            |= oneOf
                [ keyword2 Up "up"
                , keyword2 Down "down"
                , keyword2 Left "left"
                , keyword2 Right "right"
                ]
        )


numTokenParser : Parser Token
numTokenParser =
    toTokenParser
        (succeed (Num.fromInt >> NUM)
            |= oneOf
                [ succeed negate
                    |. symbol "-"
                    |= int
                , int
                ]
        )


prefixLabelTokenParser : Parser Token
prefixLabelTokenParser =
    toTokenParser
        (succeed PrefixLabel
            |= labelVariable
        )
        |. spaces
        |. symbol ":"



--|. commit ()


labelVariable : Parser String
labelVariable =
    variable
        { start = Char.isAlphaNum
        , inner = Char.isAlphaNum
        , reserved = Set.empty
        }


toTokenParser : Parser TokenTyp -> Parser Token
toTokenParser ttp =
    succeed
        (\startCol startOffset tokenType endOffset src ->
            Token
                tokenType
            <|
                Loc startCol
                    (String.slice startOffset endOffset src)
        )
        |= getCol
        |= getOffset
        |= ttp
        |= getOffset
        |= getSource


keyword2 tag string =
    succeed tag |. keyword string


isWordChar : Char -> Bool
isWordChar c =
    c /= ' ' && c /= '#'
