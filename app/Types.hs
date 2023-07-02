module Types where

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (StateT)
import Environment (Environment)

-- TODO Read these articles again and decide if this monad stack (StateT / ExceptT over IO)
-- really is the best design.... :-)
-- https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/
-- https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell/
-- https://www.tweag.io/blog/2020-04-16-exceptions-in-haskell/
type Interpreter value a = ReaderT Int (ExceptT (Error value) (StateT (Environment value) IO)) a

data Error a
    = Error
        { line :: Int
        , description :: String
        }
    | ReturnValue
        {value :: a}
    deriving (Show)
