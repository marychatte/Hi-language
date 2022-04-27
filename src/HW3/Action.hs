{-# LANGUAGE DerivingVia #-}

module HW3.Action
 ( HiPermission(..), 
   PermissionException(..), 
   HIO(..),
 )
where

import           Control.Exception.Base (Exception, throwIO)
import           Control.Monad.Reader   (ReaderT (..))
import qualified Data.ByteString        as B
import           Data.Functor           (($>))
import qualified Data.Sequence          as S
import           Data.Set.Internal      (Set, member)
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8')
import           Data.Time.Clock.POSIX  (getCurrentTime)
import           HW3.Base               (HiAction (..), HiMonad (..),
                                         HiValue (..))
import           System.Directory       (createDirectory, doesDirectoryExist,
                                         getCurrentDirectory, listDirectory,
                                         setCurrentDirectory)
import           System.Random

data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Ord, Eq, Bounded, Enum)

newtype PermissionException = PermissionRequired HiPermission
  deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO {runHIO :: Set HiPermission -> IO a}
  deriving
    (Functor, Monad, Applicative)
    via (ReaderT (Set HiPermission) IO)

instance HiMonad HIO where
  runAction action =
    HIO
      ( \set -> case action of
          HiActionRead path -> checkAction set AllowRead (readAction path)
          HiActionWrite path bytes -> checkAction set AllowWrite (writeAction path bytes)
          HiActionMkDir path -> checkAction set AllowWrite (mkdirAction path)
          HiActionChDir path -> checkAction set AllowRead (cdAction path)
          HiActionCwd -> checkAction set AllowRead cwdAction
          HiActionNow -> checkAction set AllowTime nowAction
          HiActionRand from to -> randAction from to
          HiActionEcho text -> checkAction set AllowWrite (echoAction text)
      )

checkAction :: Set HiPermission -> HiPermission -> IO a -> IO a
checkAction set permission action =
  if member permission set
    then action
    else throwIO (PermissionRequired permission)

cwdAction :: IO HiValue
cwdAction = HiValueString . T.pack <$> getCurrentDirectory

cdAction :: FilePath -> IO HiValue
cdAction path = setCurrentDirectory path $> HiValueNull

mkdirAction :: FilePath -> IO HiValue
mkdirAction path = createDirectory path $> HiValueNull

writeAction :: FilePath -> B.ByteString -> IO HiValue
writeAction path bytes = B.writeFile path bytes $> HiValueNull

readAction :: FilePath -> IO HiValue
readAction path = do
  existDir <- doesDirectoryExist path
  if existDir
    then do
      dirs <- listDirectory path
      return (HiValueList (S.fromList (map (HiValueString . T.pack) dirs)))
    else do
      fileContent <- B.readFile path
      case decodeUtf8' fileContent of
        Left _  -> return (HiValueBytes fileContent)
        Right t -> return (HiValueString t)

nowAction :: IO HiValue
nowAction = HiValueTime <$> getCurrentTime

randAction :: Int -> Int -> IO HiValue
randAction from to = HiValueNumber . toRational <$> randomRIO (from, to)

echoAction :: T.Text -> IO HiValue
echoAction text = putStrLn (T.unpack text) $> HiValueNull
