{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs          #-}

module HW3.Evaluator
 (eval)
where

import           Codec.Compression.Zlib
import           Codec.Serialise
import           Control.Monad.Except
import           Control.Monad.Trans.Except (throwE)
import qualified Data.ByteString            as B
import           Data.ByteString.Lazy.Char8 (fromStrict, toStrict)
import           Data.Foldable              (toList)
import           Data.Functor
import           Data.Scientific            (floatingOrInteger,
                                             fromRationalRepetendUnlimited)
import           Data.Semigroup             (stimes)
import qualified Data.Sequence              as S
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8', encodeUtf8)
import           Data.Time.Clock            (UTCTime, addUTCTime, diffUTCTime)
import qualified Data.Word                  as W
import           HW3.Base                   (HiAction (..), HiError (..),
                                             HiExpr (..), HiFun (..),
                                             HiMonad (..), HiValue (..))
import           Text.Read                  (readMaybe)

type ExprExcept m a = ExceptT HiError m a

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT (evalHiExpr expr)

-- | evaluate expressions. Ex: div(add(10, 15.1), 3) = 83 + 1/15, 123 = 123
evalHiExpr :: HiMonad m => HiExpr -> ExprExcept m HiValue
evalHiExpr (HiExprValue value) = evalHiValue value
evalHiExpr (HiExprApply (HiExprValue (HiValueFunction HiFunIf)) applyArgs) = lazyIf applyArgs
evalHiExpr (HiExprApply (HiExprValue (HiValueFunction HiFunAnd)) applyArgs) = lazyAnd applyArgs
evalHiExpr (HiExprApply (HiExprValue (HiValueFunction HiFunOr)) applyArgs) = lazyOr applyArgs
evalHiExpr (HiExprApply applyFun applyArgs) = do
  fun <- evalHiExpr applyFun
  args <- mapM evalHiExpr applyArgs
  evalHiExprApply fun args
evalHiExpr (HiExprRun run) = evalHiExprRun run

-- | lazy evaluate if function. Ex: if(true, add, mul) = add
lazyIf :: HiMonad m => [HiExpr] -> ExprExcept m HiValue
lazyIf [condState, thenState, elseState] = do
  condVal <- evalHiExpr condState
  case condVal of
    HiValueBool cond -> if cond then evalHiExpr thenState else evalHiExpr elseState
    _ -> throwE HiErrorInvalidArgument
lazyIf _ =  throwE HiErrorArityMismatch

-- | lazy evaluate and function. Ex: if(true, add, mul) = add
lazyAnd :: HiMonad m => [HiExpr] -> ExprExcept m HiValue
lazyAnd [firstWrap, second] = do
  first <- evalHiExpr firstWrap
  case first of
    evaluated@(HiValueBool False) -> return evaluated
    evaluated@HiValueNull         -> return evaluated
    _                             -> evalHiExpr second
lazyAnd _ =  throwE HiErrorArityMismatch

-- | lazy evaluate and function. Ex: if(true, add, mul) = add
lazyOr :: HiMonad m => [HiExpr] -> ExprExcept m HiValue
lazyOr [firstWrap, second] = do
  first <- evalHiExpr firstWrap
  case first of
    HiValueBool False -> evalHiExpr second
    HiValueNull       -> evalHiExpr second
    evaluated         -> return evaluated
lazyOr _ =  throwE HiErrorArityMismatch

-- | evaluate run expressions. Ex: cd("mydir")!
evalHiExprRun :: HiMonad m => HiExpr -> ExprExcept m HiValue
evalHiExprRun expr = do
  res <- evalHiExpr expr
  case res of
    (HiValueAction action) -> lift (runAction action)
    _                      -> throwE HiErrorInvalidArgument

-- | evaluate HiValue. Ex: -1.618 = -1.618, mul =  mul, true = true
evalHiValue :: HiMonad m => HiValue -> ExprExcept m HiValue
evalHiValue = return

-- | evaluate function application. Ex: sub(add(10, 15.1), 3) = 22.1, mul(1, 2) = 2
evalHiExprApply :: HiMonad m => HiValue -> [HiValue] -> ExprExcept m HiValue
evalHiExprApply (HiValueNumber _) _ = throwE HiErrorInvalidFunction
evalHiExprApply (HiValueBool _) _ = throwE HiErrorInvalidFunction
evalHiExprApply HiValueNull _ = throwE HiErrorInvalidFunction
evalHiExprApply (HiValueTime _) _ = throwE HiErrorInvalidFunction
evalHiExprApply (HiValueAction _) _ = throwE HiErrorInvalidFunction
evalHiExprApply (HiValueString text) args = evalHiValueString text args
evalHiExprApply (HiValueList list) args = evalHiValueList list args
evalHiExprApply (HiValueBytes list) args = evalHiValueBytes list args
evalHiExprApply (HiValueFunction fun) args = case fun of
  HiFunDiv -> evalDiv args
  HiFunMul -> evalMul args
  HiFunAdd -> evalAdd args
  HiFunSub -> evalSub args
  HiFunNot -> evalFunction (== 1) checkForBools (evalBoolOp (not . head)) args
  HiFunAnd -> evalFunction (== 2) checkForBools (evalBoolOp and) args
  HiFunOr -> evalFunction (== 2) checkForBools (evalBoolOp or) args
  HiFunLessThan -> evalFunction (== 2) noCheck (evalComparisonOp (<)) args
  HiFunGreaterThan -> evalFunction (== 2) noCheck (evalComparisonOp (>)) args
  HiFunEquals -> evalFunction (== 2) noCheck (evalComparisonOp (==)) args
  HiFunNotLessThan -> evalFunction (== 2) noCheck (evalComparisonOp (>=)) args
  HiFunNotGreaterThan -> evalFunction (== 2) noCheck (evalComparisonOp (<=)) args
  HiFunNotEquals -> evalFunction (== 2) noCheck (evalComparisonOp (/=)) args
  HiFunIf -> evalFunction (== 3) checkForFirstBoolAnotherEverything ifFunction args
  HiFunLength -> evalLength args
  HiFunToUpper -> evalFunction (== 1) checkForText (return . HiValueString . T.toUpper) args
  HiFunToLower -> evalFunction (== 1) checkForText (return . HiValueString . T.toLower) args
  HiFunReverse -> evalReverse args
  HiFunTrim -> evalFunction (== 1) checkForText (return . HiValueString . T.strip) args
  HiFunList -> evalFunction (const True) noCheck (return . HiValueList . S.fromList) args
  HiFunRange -> evalFunction (== 2) checkForNumbers evalRange args
  HiFunFold -> evalFunction (== 2) checkForFirstFunSecondList evalFold args
  HiFunPackBytes -> evalFunction (== 1) checkForListOfBytes (return . HiValueBytes . B.pack) args
  HiFunUnpackBytes -> evalFunction (== 1) checkForByteString evalUnpackBytes args
  HiFunEncodeUtf8 -> evalFunction (== 1) checkForText (return . HiValueBytes . encodeUtf8) args
  HiFunDecodeUtf8 -> evalFunction (== 1) checkForByteString evalDecodeUtf8 args
  HiFunZip -> evalFunction (== 1) checkForByteString evalZip args
  HiFunUnzip -> evalFunction (== 1) checkForByteString evalUnzip args
  HiFunSerialise -> evalFunction (== 1) checkForSingleValue (return . HiValueBytes . toStrict . serialise) args
  HiFunDeserialise -> evalFunction (== 1) checkForByteString evalDeserialise args
  HiFunRead -> evalFunction (== 1) checkForText (evalActionGetString HiActionRead . T.unpack) args
  HiFunWrite -> evalFunction (== 2) checkForTexts evalWrite args
  HiFunMkDir -> evalFunction (== 1) checkForText (evalActionGetString HiActionMkDir . T.unpack) args
  HiFunChDir -> evalFunction (== 1) checkForText (evalActionGetString HiActionChDir . T.unpack) args
  HiFunParseTime -> evalFunction (== 1) checkForText evalParseTime args
  HiFunRand -> evalFunction (== 2) checkForNumbers evalRand args
  HiFunEcho -> evalFunction (== 1) checkForText (evalActionGetString HiActionEcho) args

-- | evaluate application on HiValueString.
-- Ex: "meow"(0) = "m", "meow"(0,2) = "me"
evalHiValueString :: HiMonad m => T.Text -> [HiValue] -> ExprExcept m HiValue
evalHiValueString =
  evalSlicesAndIndexes
    T.length
    (T.pack "")
    T.drop
    T.take
    (\text pos -> T.singleton (T.index text pos))
    HiValueString
    HiValueString

-- | evaluate application on HiValueList.
-- Ex: [1, 2, 3](0) = 1, [1, 2, 3](0,2) = [ 1, 2 ]
evalHiValueList :: HiMonad m => S.Seq HiValue -> [HiValue] -> ExprExcept m HiValue
evalHiValueList =
  evalSlicesAndIndexes
    S.length
    S.empty
    S.drop
    S.take
    S.index
    HiValueList
    id

-- | evaluate application on HiValueBytes.
-- Ex: [# 01 3f ec#](0) = [# 01 #], [# 01 3f ec#](0,2) = [# 01 3f #]
evalHiValueBytes :: HiMonad m => B.ByteString -> [HiValue] -> ExprExcept m HiValue
evalHiValueBytes =
  evalSlicesAndIndexes
    B.length
    B.empty
    B.drop
    B.take
    (\bs pos -> fromInteger (toInteger (B.index bs pos)))
    HiValueBytes
    HiValueNumber

-- | evaluate function reverse.
-- Ex:  reverse("meow") = "woem", reverse([1, 2]) = [ 2, 1 ]
evalReverse :: HiMonad m => [HiValue] -> ExprExcept m HiValue
evalReverse =
  evalFunction (== 1) checkForText (return . HiValueString . T.reverse)
    `orTryEval` evalFunction (== 1) checkForList (return . HiValueList . S.reverse)

-- | evaluate function range.
-- Ex:  range(0, 3) = [ 0, 1, 2, 3 ]
evalRange :: HiMonad m => [Rational] -> ExprExcept m HiValue
evalRange [f, s] =
  return (HiValueList (S.fromList (fmap HiValueNumber [f .. s])))
evalRange _ = throwE HiErrorInvalidArgument

-- | evaluate function fold.
-- Ex:  fold(add, [11, 22, 33]) = 66
evalFold :: HiMonad m => [HiValue] -> ExprExcept m HiValue
evalFold [_, emptyList@(HiValueList S.Empty)] = return emptyList
evalFold [fun, HiValueList (start S.:<| list)] =
  foldM (\v1 v2 -> evalHiExprApply fun [v1, v2]) start list
evalFold _ = throwE HiErrorInvalidArgument

-- | evaluate function add.
-- Ex: 1+2 = 3, "me" + "ow" = "meow", add([1], [2]) = [ 1, 2 ], [# 00 #] + [# #] = [# 00 #],
-- parse-time("2021-01-01 00:00:00 UTC") + 365 * 24 * 60 * 60 = parse-time(2022-01-01 00:00:00 UTC)
evalAdd :: HiMonad m => [HiValue] -> ExprExcept m HiValue
evalAdd =
  evalFunction (== 2) checkForNumbers (evalArithmeticOp (+))
    `orTryEval` evalFunction (== 2) checkForTexts (return . HiValueString . T.concat)
    `orTryEval` evalFunction (== 2) checkForLists (return . HiValueList . foldl1 (S.><))
    `orTryEval` evalFunction (== 2) checkForByteStrings (return . HiValueBytes . B.concat)
    `orTryEval` evalFunction (== 2) checkForFirstTimeSecondInt evalAddTimeAndInt
  where
    evalAddTimeAndInt :: HiMonad m => [HiValue] -> ExprExcept m HiValue
    evalAddTimeAndInt [HiValueTime time, HiValueNumber int] =
      return (HiValueTime (addUTCTime (fromIntegral (fromEnum int)) time))
    evalAddTimeAndInt _ = throwE HiErrorInvalidArgument

-- | evaluate function sub.
-- Ex: 1 - -1 = 2,
-- parse-time("2021-12-15 00:37:51.000890793 UTC") -
-- parse-time("2021-12-15 00:37:47.649047038 UTC") = 3.351843755
evalSub :: HiMonad m => [HiValue] -> ExprExcept m HiValue
evalSub =
  evalFunction (== 2) checkForNumbers (evalArithmeticOp (-))
    `orTryEval` evalFunction (== 2) checkForTimes evalSubTimes
  where
    evalSubTimes :: HiMonad m => [UTCTime] -> ExprExcept m HiValue
    evalSubTimes [time1, time2] = return (HiValueNumber (toRational (diffUTCTime time1 time2)))
    evalSubTimes _ = throwE HiErrorInvalidArgument

-- | evaluate function length.
-- Ex: length("meow") = 4, length([1, 2]) = 2, length([# #]) = 0
evalLength :: HiMonad m => [HiValue] -> ExprExcept m HiValue
evalLength =
  evalFunction (== 1) checkForText (evalLength' T.length)
    `orTryEval` evalFunction (== 1) checkForList (evalLength' S.length)
    `orTryEval` evalFunction (== 1) checkForByteString (evalLength' B.length)
  where
    evalLength' lenFun = return . HiValueNumber . toRational . lenFun

-- | evaluate function mul.
-- Ex: 1 * 2 = 2, [1, 2] * 2 = [ 1, 2, 1, 2 ], [# 12 #] * 2 = [# 12 12 #]
evalMul :: HiMonad m => [HiValue] -> ExprExcept m HiValue
evalMul =
  evalFunction (== 2) checkForNumbers (evalArithmeticOp (*))
    `orTryEval`
      evalFunction (== 2) checkForFirstTextSecondInt (evalStimes getString HiValueString)
    `orTryEval`
      evalFunction (== 2) checkForFirstListSecondInt (evalStimes getList HiValueList)
    `orTryEval`
      evalFunction (== 2) checkForFirstByteSecondInt (evalStimes getBytes HiValueBytes)
  where
    evalStimes ::
      (HiMonad m, Semigroup a) =>
      (HiValue -> ExprExcept m a) ->
      (a -> HiValue) ->
      [HiValue] ->
      ExprExcept m HiValue
    evalStimes extract constr [argWrap, num] =
      integerFromRational num
        >>= ( \times ->
                if times <= 0
                  then throwE HiErrorInvalidArgument
                  else do
                    arg <- extract argWrap
                    return (constr (stimes times arg))
            )
    evalStimes _ _ _ = throwE HiErrorInvalidArgument
    getString (HiValueString str) = return str
    getString _                   = throwE HiErrorInvalidArgument
    getList (HiValueList list) = return list
    getList _                  = throwE HiErrorInvalidArgument
    getBytes (HiValueBytes bytes) = return bytes
    getBytes _                    = throwE HiErrorInvalidArgument

-- | evaluate function div.
-- Ex: 4/3 = 1 + 1/3, "me" / "ow" = "me/ow"
evalDiv :: HiMonad m => [HiValue] -> ExprExcept m HiValue
evalDiv =
  evalFunction (== 2) checkForTexts (return . HiValueString . T.intercalate (T.pack "/"))
    `orTryEval` evalFunction (== 2) checkForNumbers divNumbers
  where
    divNumbers :: HiMonad m => [Rational] -> ExprExcept m HiValue
    divNumbers list@[_, num] =
      if num == 0.0
      then throwE HiErrorDivideByZero
      else evalArithmeticOp (/) list
    divNumbers _ = throwE HiErrorInvalidArgument
-- | evaluate if function. Ex: if(true, add, mul) = add
ifFunction :: HiMonad m => [HiValue] -> ExprExcept m HiValue
ifFunction [HiValueBool cond, ifState, elseState] =
  if cond
    then return ifState
    else return elseState
ifFunction _ = throwE HiErrorInvalidArgument

-- | evaluate function decodeUtf8.
-- Ex: decode-utf8([# 48 65 6c 6c 6f #]) = "Hello"
evalDecodeUtf8 :: HiMonad m => B.ByteString -> ExprExcept m HiValue
evalDecodeUtf8 b = case decodeUtf8' b of
  (Left _)  -> return HiValueNull
  (Right t) -> return (HiValueString t)

-- | evaluate function unpack-bytes.
-- Ex: unpack-bytes([# 10 20 30 #]) = [ 16, 32, 48 ]
evalUnpackBytes :: HiMonad m => B.ByteString -> ExprExcept m HiValue
evalUnpackBytes bytes =
  return
    (HiValueList (fmap (HiValueNumber . toRational) (S.fromList $ B.unpack bytes)))

-- | evaluate function zip.
-- Ex: zip(encode-utf8("Hello, World!")) =
--[# 78 da f3 48 cd c9 c9 d7 51 08 cf 2f ca 49 51 04 00 1f 9e 04 6a #]
-- unzip(zip(A)) = A
evalZip :: HiMonad m => B.ByteString -> ExprExcept m HiValue
evalZip =
  return . HiValueBytes . toStrict
    . compressWith defaultCompressParams {compressLevel = bestCompression}
    . fromStrict

-- | evaluate function unzip.
-- Ex: unzip([# 78 da 63 64 62 06 00 00 0d 00 07 #]) = [# 01 02 03 #]
-- unzip(zip(A)) = A
evalUnzip :: HiMonad m => B.ByteString -> ExprExcept m HiValue
evalUnzip = return . HiValueBytes . toStrict . decompress . fromStrict

-- | evaluate function deserialise.
--  deserialise(serialise(A)) ≡ A
evalDeserialise :: HiMonad m => B.ByteString -> ExprExcept m HiValue
evalDeserialise bytes =
  case deserialiseOrFail (fromStrict bytes) of
    Left _    -> return HiValueNull
    Right val -> return val

-- | evaluate actions which accept FilePath.
-- Just wrap it as action. Ex: read("hi.txt") = read("hi.txt")
evalActionGetString :: HiMonad m => (a -> HiAction) -> a -> ExprExcept m HiValue
evalActionGetString constr = return . HiValueAction . constr

-- | evaluate action write.
-- Ex: write("tmp/hi.txt", "Hello") = write("tmp/hi.txt", "Hello")!
evalWrite :: HiMonad m => [T.Text] -> ExprExcept m HiValue
evalWrite [path, text] =
  return (HiValueAction (HiActionWrite (T.unpack path) (encodeUtf8 text)))
evalWrite _ = throwE HiErrorInvalidArgument

-- | evaluate action parse-time.
-- Ex: parse-time("2021-01-01 00:00:00 UTC") = parse-time("2021-01-01 00:00:00 UTC")
evalParseTime :: HiMonad m => T.Text -> ExprExcept m HiValue
evalParseTime str =
  case readMaybe (T.unpack str) of
    Nothing     -> return HiValueNull
    (Just time) -> return (HiValueTime time)

-- | evaluate action rand.
-- Ex: rand(0, 10) = rand(0, 10)
evalRand :: HiMonad m => [Rational] -> ExprExcept m HiValue
evalRand [from, to] = return (HiValueAction (HiActionRand (fromEnum from) (fromEnum to)))
evalRand _ = throwE HiErrorInvalidArgument

-- | -------------------------- Help Functions -------------------------- | --

-- | -------------------------- Evaluator helpers
-- | evaluate functions. Input:
-- arity of function,
-- function that check input args for types and return args in expected type
-- function for evaluation
-- input arguments
evalFunction ::
  HiMonad m =>
  (Int -> Bool) ->
  ([HiValue] -> ExprExcept m b) ->
  (b -> ExprExcept m HiValue) ->
  [HiValue] ->
  ExprExcept m HiValue
evalFunction checkArity expectArgs fun wrapArgs =
  if checkArity $ length wrapArgs
    then expectArgs wrapArgs >>= fun
    else throwE HiErrorArityMismatch

-- | evaluate slices and indexes
evalSlicesAndIndexes ::
  HiMonad m =>
  (a -> Int) ->
  a ->
  (Int -> a -> a) ->
  (Int -> a -> a) ->
  (a -> Int -> b) ->
  (a -> HiValue) ->
  (b -> HiValue) ->
  a ->
  [HiValue] ->
  ExprExcept m HiValue
evalSlicesAndIndexes
  lengthFun
  emptyVal
  dropFun
  takeFun
  indexFun
  constrSlices
  constrIndexes
  val =
    evalFunction (== 2) checkForNumbersAndNulls evalSlices
      `orTryEval` evalFunction (== 1) checkForInteger evalIndex
    where
      evalSlices [s, e] =
        let lengthOfVal = lengthFun val
            processNum num =
              let n = fromEnum num
               in if n < 0 then lengthOfVal + n else n
            start = case s of
              (HiValueNumber num) -> processNum num
              _                   -> 0
            end = case e of
              (HiValueNumber num) -> processNum num
              _                   -> lengthOfVal
         in if start >= end
              then return (constrSlices emptyVal)
              else return (constrSlices (dropFun start (takeFun end val)))
      evalSlices _ = throwE HiErrorInvalidArgument

      evalIndex pos =
        if pos < 0 || pos >= lengthFun val
          then return HiValueNull
          else return (constrIndexes (indexFun val pos))

-- | evaluate arithmetic functions. Ex: sub, add
evalArithmeticOp :: HiMonad m => (Rational -> Rational -> Rational) -> [Rational] -> ExprExcept m HiValue
evalArithmeticOp op args = return (HiValueNumber (foldr1 op args))

-- | evaluate bool functions. Ex: and, or
evalBoolOp :: HiMonad m => ([Bool] -> Bool) -> [Bool] -> ExprExcept m HiValue
evalBoolOp op args = return (HiValueBool (op args))

-- | evaluate comparison functions. Ex: equals, greater-than
evalComparisonOp :: HiMonad m => (HiValue -> HiValue -> Bool) -> [HiValue] -> ExprExcept m HiValue
evalComparisonOp op [val1, val2] = return (HiValueBool (op val1 val2))
evalComparisonOp _ _             = throwE HiErrorInvalidArgument

-- | try to eval in first way, if fail then try to eval second way,
-- else return result of first way
orTryEval ::
  HiMonad m =>
  ([HiValue] -> ExprExcept m HiValue) ->
  ([HiValue] -> ExprExcept m HiValue) ->
  ([HiValue] -> ExprExcept m HiValue)
orTryEval eval1 eval2 args = catchError (eval1 args) (\_ -> eval2 args)

-- | extract int from HiValueNumber
integerFromRational :: HiMonad m => HiValue -> ExprExcept m Int
integerFromRational (HiValueNumber rational) =
  case fromRationalRepetendUnlimited rational of
    (scientific, Nothing) ->
      case floatingOrInteger scientific :: Either Double Integer of
        (Left _)    -> throwE HiErrorInvalidArgument
        (Right int) -> return (fromIntegral (int :: Integer))
    _ -> throwE HiErrorInvalidArgument
integerFromRational _ = throwE HiErrorInvalidArgument

-- | -------------------------- Checkers of arguments

-- | check that args is single int
checkForInteger :: HiMonad m => [HiValue] -> ExprExcept m Int
checkForInteger [num@(HiValueNumber _)] = integerFromRational num
checkForInteger _                       = throwE HiErrorInvalidArgument

-- | check that all args are numbers or nulls
checkForNumbersAndNulls :: HiMonad m => [HiValue] -> ExprExcept m [HiValue]
checkForNumbersAndNulls = mapM checkForIntegerAndNull
  where
    checkForIntegerAndNull n@(HiValueNumber _) =
      integerFromRational n <&> (HiValueNumber . toRational)
    checkForIntegerAndNull HiValueNull = return HiValueNull
    checkForIntegerAndNull _ = throwE HiErrorInvalidArgument

-- | check that all args are numbers
checkForNumbers :: HiMonad m => [HiValue] -> ExprExcept m [Rational]
checkForNumbers = mapM checkForNumber
  where
    checkForNumber (HiValueNumber num) = return num
    checkForNumber _                   = throwE HiErrorInvalidArgument

-- | check that all args are bools
checkForBools :: HiMonad m => [HiValue] -> ExprExcept m [Bool]
checkForBools = mapM checkForBool
  where
    checkForBool (HiValueBool bool) = return bool
    checkForBool _                  = throwE HiErrorInvalidArgument

-- | check that all args are texts
checkForTexts :: HiMonad m => [HiValue] -> ExprExcept m [T.Text]
checkForTexts = mapM (checkForText . (: []))

-- | check that args is single text
checkForText :: HiMonad m => [HiValue] -> ExceptT HiError m T.Text
checkForText [HiValueString text] = return text
checkForText _                    = throwE HiErrorInvalidArgument

-- | check that all args are lists
checkForList :: HiMonad m => [HiValue] -> ExceptT HiError m (S.Seq HiValue)
checkForList [HiValueList list] = return list
checkForList _                  = throwE HiErrorInvalidArgument

-- | check that args is single bytestring
checkForByteString :: HiMonad m => [HiValue] -> ExceptT HiError m B.ByteString
checkForByteString [HiValueBytes list] = return list
checkForByteString _                   = throwE HiErrorInvalidArgument

-- | allows all types of args
noCheck :: HiMonad m => [HiValue] -> ExprExcept m [HiValue]
noCheck = mapM return

-- | check that first arg is bool and allows all types of next two args
checkForFirstBoolAnotherEverything :: HiMonad m => [HiValue] -> ExprExcept m [HiValue]
checkForFirstBoolAnotherEverything list@[HiValueBool _, _, _] = return list
checkForFirstBoolAnotherEverything _ = throwE HiErrorInvalidArgument

-- | check that first arg is given type and second arg is number
checkForFirstTypeSecondInt ::
  HiMonad m =>
  ([HiValue] -> ExprExcept m a) ->
  [HiValue] ->
  ExprExcept m [HiValue]
checkForFirstTypeSecondInt check list@[val, num] =
  (integerFromRational num *> check [val]) $> list
checkForFirstTypeSecondInt _ _ = throwE HiErrorInvalidArgument

-- | check that first arg is text and second arg is number
checkForFirstTextSecondInt :: HiMonad m => [HiValue] -> ExprExcept m [HiValue]
checkForFirstTextSecondInt = checkForFirstTypeSecondInt checkForText

-- | check that first arg is list and second arg is number
checkForFirstListSecondInt :: HiMonad m => [HiValue] -> ExprExcept m [HiValue]
checkForFirstListSecondInt = checkForFirstTypeSecondInt checkForList

-- | check that first arg is bytestring and second arg is number
checkForFirstByteSecondInt :: HiMonad m => [HiValue] -> ExprExcept m [HiValue]
checkForFirstByteSecondInt = checkForFirstTypeSecondInt checkForByteString

-- | check that first arg is time and second arg is number
checkForFirstTimeSecondInt :: HiMonad m => [HiValue] -> ExprExcept m [HiValue]
checkForFirstTimeSecondInt = checkForFirstTypeSecondInt checkForTime

-- | check that first arg is fun and second arg is list
checkForFirstFunSecondList :: Monad m => [HiValue] -> ExprExcept m [HiValue]
checkForFirstFunSecondList list@[HiValueFunction _, HiValueList _] = return list
checkForFirstFunSecondList _ = throwE HiErrorInvalidArgument

-- | check that args are lists
checkForLists :: HiMonad m => [HiValue] -> ExprExcept m [S.Seq HiValue]
checkForLists = mapM (checkForList . (: []))

-- | check that args are bytestrings
checkForByteStrings :: HiMonad m => [HiValue] -> ExprExcept m [B.ByteString]
checkForByteStrings = mapM (checkForByteString . (: []))

-- | check that all args are times
checkForTimes :: HiMonad m => [HiValue] -> ExprExcept m [UTCTime]
checkForTimes = mapM (checkForTime . (: []))

-- | check that args is single time
checkForTime :: HiMonad m => [HiValue] -> ExprExcept m UTCTime
checkForTime [HiValueTime time] = return time
checkForTime _                  = throwE HiErrorInvalidArgument

-- | check that args is single time
checkForSingleValue :: HiMonad m => [HiValue] -> ExprExcept m HiValue
checkForSingleValue [val] = return val
checkForSingleValue _                  = throwE HiErrorInvalidArgument

-- | check that args is a list of right int (bytes)
checkForListOfBytes :: HiMonad m => [HiValue] -> ExprExcept m [W.Word8]
checkForListOfBytes [HiValueList list] = mapM checkForInt256 (toList list)
  where
    checkForInt256 :: HiMonad m => HiValue -> ExprExcept m W.Word8
    checkForInt256 n@(HiValueNumber _) = do
      num <- integerFromRational n
      if 0 <= num && num < 256
        then return (fromIntegral num)
        else throwE HiErrorInvalidArgument
    checkForInt256 _ = throwE HiErrorInvalidArgument
checkForListOfBytes _ = throwE HiErrorInvalidArgument
