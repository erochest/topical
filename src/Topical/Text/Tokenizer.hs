{-# LANGUAGE OverloadedStrings #-}


module Topical.Text.Tokenizer
    ( splitTokenizer
    , parserTokenizer
    ) where


import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Char            (isSpace)
import qualified Data.Text            as T

import           Topical.Text.Types


splitTokenizer :: Tokenizer
splitTokenizer = T.split isSpace

parserTokenizer :: Parser T.Text -> Tokenizer
parserTokenizer p = either (const []) id . parseOnly (many (just p anyChar))
    where just p' s = p <|> (s *> just p' s)
