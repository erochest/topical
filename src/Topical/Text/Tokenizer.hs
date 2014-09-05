{-# LANGUAGE OverloadedStrings #-}


module Topical.Text.Tokenizer
    ( splitTokenizer
    ) where


import           Data.Char          (isSpace)
import qualified Data.Text          as T

import           Topical.Text.Types


splitTokenizer :: Tokenizer
splitTokenizer = T.split isSpace

