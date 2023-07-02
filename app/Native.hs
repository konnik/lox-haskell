module Native (
  clock,
  readStr,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Value (Value (..))

readStr :: Value
readStr = CallableVal 0 "readStr" $ \_args -> liftIO $ fmap StrVal getLine

clock :: Value
clock = CallableVal 0 "clock" $ \_args -> do
  liftIO $ fmap NumVal getCurrentTimeSeconds
 where
  getCurrentTimeSeconds :: IO Double
  getCurrentTimeSeconds = do
    posixTime <- getPOSIXTime
    return $ realToFrac posixTime
