module Native (
  clock,
  readStr,
  termWidth,
  termHeight,
  cls,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified System.Console.Terminal.Size
import System.Info (os)
import System.Process (system)
import Value (FuncType (..), Value (..))

readStr :: Value
readStr = CallableVal 0 Native "readStr" $ \_args -> liftIO $ fmap StrVal getLine

cls :: Value
cls = CallableVal 0 Native "cls" $ \_args -> liftIO $ clearTerminal >> pure NilVal
 where
  clearTerminal :: IO ()
  clearTerminal = do
    case os of
      "linux" -> system "clear" >> return ()
      "darwin" -> system "clear" >> return ()
      "mingw32" -> system "cls" >> return ()
      _ -> return ()

termWidth :: Value
termWidth = CallableVal 0 Native "termWidth" $ \_args -> liftIO $ widthVal
 where
  widthVal :: IO Value
  widthVal = do
    maybeSize <- System.Console.Terminal.Size.size :: IO (Maybe (System.Console.Terminal.Size.Window Int))
    case maybeSize of
      Nothing -> pure NilVal
      Just (System.Console.Terminal.Size.Window{width}) -> pure $ NumVal $ fromIntegral width

termHeight :: Value
termHeight = CallableVal 0 Native "termHeight" $ \_args -> liftIO $ heightVal
 where
  heightVal :: IO Value
  heightVal = do
    maybeSize <- System.Console.Terminal.Size.size :: IO (Maybe (System.Console.Terminal.Size.Window Int))
    case maybeSize of
      Nothing -> pure NilVal
      Just (System.Console.Terminal.Size.Window{height}) -> pure $ NumVal $ fromIntegral height

clock :: Value
clock = CallableVal 0 Native "clock" $ \_args -> do
  liftIO $ fmap NumVal getCurrentTimeSeconds
 where
  getCurrentTimeSeconds :: IO Double
  getCurrentTimeSeconds = do
    posixTime <- getPOSIXTime
    return $ realToFrac posixTime
