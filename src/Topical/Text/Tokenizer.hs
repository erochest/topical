{-# LANGUAGE OverloadedStrings #-}


module Topical.Text.Tokenizer
    ( splitTokenizer
    ) where


import           Data.Char (isSpace)
import qualified Data.Text as T


splitTokenizer :: T.Text -> [T.Text]
splitTokenizer = T.split isSpace

