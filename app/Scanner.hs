{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Scanner where

import Result
import Token

scanTokens :: String -> Result [Token]
scanTokens source =
    okResult [Token{type_ = COMMA, lexeme = ",", literal = (), line = 42}]
