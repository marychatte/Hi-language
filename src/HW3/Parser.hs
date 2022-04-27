{-# LANGUAGE GADTs #-}

module HW3.Parser
 (parse)
where

import           Control.Applicative            (liftA2)
import           Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import qualified Data.ByteString                as B
import           Data.Char                      (isHexDigit, isSpace)
import           Data.Functor                   (($>))
import           Data.Text                      (pack)
import           Data.Text.Internal.Read        (hexDigitToInt)
import           Data.Void                      (Void)
import           HW3.Base                       (HiAction (..), HiExpr (..),
                                                 HiFun (..), HiValue (..))
import           Text.Megaparsec                hiding (parse)
import           Text.Megaparsec.Byte           hiding (space)
import           Text.Megaparsec.Char.Lexer     hiding (binary)

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (skipWhitespaces *> parserOperatorExpr <* eof) ""

operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ [ binaryInfixL (string "*") (getOperation HiFunMul),
      binaryInfixL (try (string "/" <* notFollowedBy (string "="))) (getOperation HiFunDiv)
    ],
    [ binaryInfixL (string "+") (getOperation HiFunAdd),
      binaryInfixL (string "-") (getOperation HiFunSub)
    ],
    [ binaryInfixN (string ">=") (getOperation HiFunNotLessThan),
      binaryInfixN (string "<=") (getOperation HiFunNotGreaterThan),
      binaryInfixN (string "<") (getOperation HiFunLessThan),
      binaryInfixN (string ">") (getOperation HiFunGreaterThan),
      binaryInfixN (string "==") (getOperation HiFunEquals),
      binaryInfixN (string "/=") (getOperation HiFunNotEquals)
    ],
    [binaryInfixR (string "&&") (getOperation HiFunAnd)],
    [binaryInfixR (string "||") (getOperation HiFunOr)]
  ]

-- | parser of expressions with infix operators.
-- Ex: 10 == 2*5 && 143 == 11*(add(2, 13))
parserOperatorExpr :: Parser HiExpr
parserOperatorExpr = makeExprParser parserTerms operatorTable

-- | parser of expressions between operators.
-- Ex: add(2, 3*4), (3 + mul(1, 2))
parserTerms :: Parser HiExpr
parserTerms =
  ( skipWhitespaces
      *> ( choice
             [ between (string "(") (string ")") parserOperatorExpr,
               parserHiExprValue,
               parserHiValueList
             ]
             >>= parserRecursiveHiExprApply
         )
      <* skipWhitespaces
  )
    >>= parserRunExpr

-- | parser of run expressions. Ex: now!
parserRunExpr :: HiExpr -> Parser HiExpr
parserRunExpr expr = do
  run <- optional (string "!")
  skipWhitespaces
  case run of
    Nothing  -> return expr
    (Just _) -> parserRecursiveHiExprApply (HiExprRun expr)
      <* skipWhitespaces
      >>= parserRunExpr

-- | parser of single HiExpressions. Ex: div, 3
parserHiExprValue :: Parser HiExpr
parserHiExprValue = HiExprValue <$> parserHiValue

-- | parser of HiValue. Ex: -1.618, mul, true
parserHiValue :: Parser HiValue
parserHiValue =
  choice
    [ parserHiValueNumber,
      parserHiValueFunction,
      parserHiValueBool,
      parserHiValueNull,
      parserHiValueString,
      parserHiValueBytes,
      parserHiValueAction
    ]

-- | parser of numbers. Ex: 2, 3.14, -1.618, 1.2e5
parserHiValueNumber :: Parser HiValue
parserHiValueNumber = HiValueNumber . toRational <$> signed skipWhitespaces scientific

-- | parser of function names. Ex: div, mul, add, sub
parserHiValueFunction :: Parser HiValue
parserHiValueFunction =
  HiValueFunction
    <$> parserChoice
      [ (HiFunDiv, "div"),
        (HiFunMul, "mul"),
        (HiFunAdd, "add"),
        (HiFunSub, "sub"),
        (HiFunAnd, "and"),
        (HiFunOr, "or"),
        (HiFunLessThan, "less-than"),
        (HiFunGreaterThan, "greater-than"),
        (HiFunEquals, "equals"),
        (HiFunNotLessThan, "not-less-than"),
        (HiFunNotGreaterThan, "not-greater-than"),
        (HiFunNotEquals, "not-equals"),
        (HiFunIf, "if"),
        (HiFunNot, "not"),
        (HiFunLength, "length"),
        (HiFunToUpper, "to-upper"),
        (HiFunToLower, "to-lower"),
        (HiFunReverse, "reverse"),
        (HiFunTrim, "trim"),
        (HiFunList, "list"),
        (HiFunRange, "range"),
        (HiFunFold, "fold"),
        (HiFunPackBytes, "pack-bytes"),
        (HiFunUnpackBytes, "unpack-bytes"),
        (HiFunZip, "zip"),
        (HiFunUnzip, "unzip"),
        (HiFunEncodeUtf8, "encode-utf8"),
        (HiFunDecodeUtf8, "decode-utf8"),
        (HiFunSerialise, "serialise"),
        (HiFunDeserialise, "deserialise"),
        (HiFunRead, "read"),
        (HiFunWrite, "write"),
        (HiFunMkDir, "mkdir"),
        (HiFunChDir, "cd"),
        (HiFunParseTime, "parse-time"),
        (HiFunRand, "rand"),
        (HiFunEcho, "echo")
      ]

-- | parser of bools. Ex: true, false
parserHiValueBool :: Parser HiValue
parserHiValueBool = HiValueBool <$> parserChoice [(True, "true"), (False, "false")]

-- | parser of null. Ex: null
parserHiValueNull :: Parser HiValue
parserHiValueNull = string "null" $> HiValueNull

-- | parser of string. Ex: "meow", "cat"
parserHiValueString :: Parser HiValue
parserHiValueString = do
  _ <- string "\""
  HiValueString . pack <$> manyTill charLiteral (string "\"")

-- | parser of list. Ex: [1, 2, 3]
parserHiValueList :: Parser HiExpr
parserHiValueList =
  HiExprApply (HiExprValue (HiValueFunction HiFunList))
    <$> parserCustomList "[" "]" id parserOperatorExpr ","

-- | parser of bytes. Ex: [# 01 3f ec #]
parserHiValueBytes :: Parser HiValue
parserHiValueBytes = do
  _ <- string "[#"
  skipWhitespaces
  args <- sepEndBy parserTwoDigitHexadecimal (some (satisfy isSpace))
  _ <- string "#]"
  return (HiValueBytes $ B.pack args)
  where
    parserTwoDigitHexadecimal =
      liftA2
        (\f s -> fromIntegral (16 * hexDigitToInt f + hexDigitToInt s))
        (satisfy isHexDigit)
        (satisfy isHexDigit)

-- | parser of action. Ex: cwd
parserHiValueAction :: Parser HiValue
parserHiValueAction =
  parserChoice
    [ (HiValueAction HiActionCwd, "cwd"),
      (HiValueAction HiActionNow, "now")
    ]

-- | -------------------------- Help Functions -------------------------- | --
--
--
-- | -------------------------- Parser

-- | skip whitespaces (>=0)
skipWhitespaces :: Parser ()
skipWhitespaces = do
  _ <- many (satisfy isSpace)
  return ()

-- | [Constructor, String] ->
-- create choise parser which parse such String in given constructor
parserChoice :: [(a, String)] -> Parser a
parserChoice list =
  choice (map (\(fun, name) -> fun <$ string name) list)

-- | parser of HiExpressions with applying arguments. Ex: div(add(10, 15.1), 3)
parserRecursiveHiExprApply :: HiExpr -> Parser HiExpr
parserRecursiveHiExprApply expr = do
  skipWhitespaces
  a <- optional (parserCustomList "(" ")" id parserOperatorExpr ",")
  case a of
    Nothing     -> return expr
    (Just args) -> parserRecursiveHiExprApply (HiExprApply expr args)

-- | parser of list.
-- get definition of start of list, definition of end,
-- function to combine lists of elements
-- parser of element
-- separator
parserCustomList ::
  String ->
  String ->
  ([a] -> b) ->
  Parser a ->
  String ->
  Parser b
parserCustomList open close combine parser sep = do
  _ <- string open
  skipWhitespaces
  args <- combine <$> parser `sepBy` string sep
  skipWhitespaces
  _ <- string close
  return args

-- | -------------------------- Operators

-- | create expression which is applying given function to two args
getOperation :: HiFun -> HiExpr -> HiExpr -> HiExpr
getOperation fun arg1 arg2 =
  HiExprApply
    (HiExprValue (HiValueFunction fun))
    [arg1, arg2]

-- | create binary operator applying on HiExpr
binary ::
  (Parser (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr) ->
  Parser String ->
  (HiExpr -> HiExpr -> HiExpr) ->
  Operator Parser HiExpr
binary opConstr parser function = opConstr (function <$ parser)

-- | create left-associative infix operator
binaryInfixL :: Parser String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
binaryInfixL = binary InfixL

-- | create right-associative infix operator
binaryInfixR :: Parser String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
binaryInfixR = binary InfixR

-- | create non-associative infix operator
binaryInfixN :: Parser String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
binaryInfixN = binary InfixN
