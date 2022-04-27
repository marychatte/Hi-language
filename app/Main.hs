module Main where

import           Control.Monad.IO.Class
import           Data.Set                 (fromList)
import           HW3.Action
import           HW3.Evaluator            (eval)
import           HW3.Parser               (parse)
import           HW3.Pretty               (prettyValue)
import           System.Console.Haskeline
import           Text.Megaparsec.Error    (errorBundlePretty)

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing -> return ()
        Just "goodbye" -> return ()
        Just input -> do
          case parse input of
            Left e -> outputStrLn (errorBundlePretty e)
            Right parsed -> do
              expr <- liftIO $ runHIO (eval parsed) (fromList [AllowRead, AllowWrite, AllowTime])
              case expr of
                Left e      -> outputStrLn (show e)
                Right value -> outputStrLn (show (prettyValue value))
          loop
