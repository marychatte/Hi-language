module HW3.Pretty
 (prettyValue)
where

import qualified Data.ByteString                        as B
import           Data.Ratio                             (denominator, numerator,
                                                         (%))
import           Data.Scientific                        (base10Exponent,
                                                         coefficient,
                                                         floatingOrInteger,
                                                         fromRationalRepetendUnlimited)
import           Data.Sequence.Internal                 (Seq (..))
import           Data.Text                              (pack, replace, unpack)
import qualified Data.Word                              as W
import           HW3.Base
import           Numeric                                (showHex)
import           Prettyprinter.Internal                 (emptyDoc, pretty,
                                                         (<+>))
import           Prettyprinter.Internal.Type            (Doc)
import           Prettyprinter.Render.Terminal.Internal (AnsiStyle)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber num) = formatNumber num
prettyValue (HiValueFunction fun) = formatFun fun
prettyValue (HiValueBool bool) = if bool then pretty "true" else pretty "false"
prettyValue HiValueNull = pretty "null"
prettyValue (HiValueString s) = formatString (unpack s)
prettyValue (HiValueList list) = pretty "[" <+> formatList list <+> pretty "]"
prettyValue (HiValueBytes byteString) = formatBytes (B.unpack byteString)
prettyValue (HiValueAction action) = formatAction action
prettyValue (HiValueTime time) = pretty "parse-time(" <> pretty (show time) <> pretty ")"

formatString :: String -> Doc ann
formatString s =
  pretty "\""
    <> pretty (replace (pack "\"") (pack "\\\"") (pack s))
    <> pretty "\""

formatNumber :: Rational -> Doc AnsiStyle
formatNumber num = case fromRationalRepetendUnlimited num of
  (scientific, Nothing) ->
    case floatingOrInteger scientific of
      (Left float) -> pretty (float :: Double)
      (Right int)  -> pretty (int :: Integer)
  (scientific, Just index) ->
    if abs numerator' < denominator'
      then pretty numerator' <> pretty "/" <> pretty denominator'
      else
        let (quot', rem') = quotRem (abs numerator') denominator'
         in if numerator' > 0
              then
                pretty quot' <+> pretty "+" <+> pretty rem'
                  <> pretty "/"
                  <> pretty denominator'
              else
                pretty "-" <> pretty quot' <+> pretty "-"
                  <+> pretty rem'
                  <> pretty "/"
                  <> pretty denominator'
    where
      m = 10 ^ ((- base10Exponent scientific) - index)
      (nonRepetend, repetend) = quotRem (coefficient scientific) m
      rational = fromInteger nonRepetend + repetend % (m - 1) / fromInteger (10 ^ index)
      numerator' = numerator rational
      denominator' = denominator rational

formatFun :: HiFun -> Doc AnsiStyle
formatFun = pretty . getStrFun
  where
    getStrFun fun = case fun of
      HiFunDiv            -> "div"
      HiFunMul            -> "mul"
      HiFunAdd            -> "add"
      HiFunSub            -> "sub"
      HiFunNot            -> "not"
      HiFunAnd            -> "and"
      HiFunOr             -> "or"
      HiFunLessThan       -> "less-than"
      HiFunGreaterThan    -> "greater-than"
      HiFunEquals         -> "equals"
      HiFunNotLessThan    -> "not-less-than"
      HiFunNotGreaterThan -> "not-greater-than"
      HiFunNotEquals      -> "not-equals"
      HiFunIf             -> "if"
      HiFunLength         -> "length"
      HiFunToUpper        -> "to-upper"
      HiFunToLower        -> "to-lower"
      HiFunReverse        -> "reverse"
      HiFunTrim           -> "trim"
      HiFunList           -> "list"
      HiFunRange          -> "range"
      HiFunFold           -> "fold"
      HiFunPackBytes      -> "pack-bytes"
      HiFunUnpackBytes    -> "unpack-bytes"
      HiFunZip            -> "zip"
      HiFunUnzip          -> "unzip"
      HiFunEncodeUtf8     -> "encode-utf8"
      HiFunDecodeUtf8     -> "decode-utf8"
      HiFunSerialise      -> "serialise"
      HiFunDeserialise    -> "deserialise"
      HiFunRead           -> "read"
      HiFunWrite          -> "write"
      HiFunMkDir          -> "mkdir"
      HiFunChDir          -> "cd"
      HiFunParseTime      -> "parse-time"
      HiFunRand           -> "rand"
      HiFunEcho           -> "echo"

formatList :: Seq HiValue -> Doc AnsiStyle
formatList Empty          = emptyDoc
formatList (el :<| Empty) = prettyValue el
formatList (el :<| list)  = prettyValue el <> pretty ", " <> formatList list

formatBytes :: [W.Word8] -> Doc AnsiStyle
formatBytes bytes =
  pretty "[#"
    <> foldl (\cur w -> cur <+> word8ToStr w) emptyDoc bytes
    <+> pretty "#]"

word8ToStr :: W.Word8 -> Doc AnsiStyle
word8ToStr w =
  let (f, s) = quotRem (toInteger w) 16
   in pretty (showHex f "") <> pretty (showHex s "")

formatAction :: HiAction -> Doc AnsiStyle
formatAction = getStrAction
  where
    getStrAction action = case action of
      (HiActionRead path) -> pretty "read" <> formatArguments "(" ")" [formatString path]
      (HiActionWrite path text) ->
        pretty "write" <> formatArguments "(" ")" [formatString path, formatBytes (B.unpack text)]
      HiActionMkDir path -> pretty "mkdir" <> formatArguments "(" ")" [formatString path]
      HiActionChDir path -> pretty "cd" <> formatArguments "(" ")" [formatString path]
      HiActionCwd -> pretty "cwd"
      HiActionNow -> pretty "now"
      HiActionRand from to -> pretty "rand" <> formatArguments "( " " )" [pretty from, pretty to]
      HiActionEcho text -> pretty "echo" <> formatArguments "(" ")" [formatString (unpack text)]
    formatArguments open end list = pretty open <> foldl1 (\f s -> f <> pretty ", " <> s) list <> pretty end
