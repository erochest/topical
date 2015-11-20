{-# LANGUAGE OverloadedStrings #-}


module Topical.Text.Regex
    ( Replace
    , rgroup
    , rtext
    , rstring
    , rfn
    , rtfn
    , rbuilder

    , replace
    , replace'
    , replaceAll
    , replaceAll'

    , parseReplace
    ) where


import           Control.Applicative
import           Data.Attoparsec.Text
import qualified Data.Text             as T

import           Data.Text.ICU.Replace


parseReplace :: T.Text -> Replace
parseReplace t = either (const $ rtext t) id
               $ parseOnly (replacement <* endOfInput) t

replacement :: Parser Replace
replacement = mconcat <$> many (dollarGroup <|> raw)

dollarGroup :: Parser Replace
dollarGroup = char '$' *> (grp <|> escaped)
    where curly   = char '{' *> decimal <* char '}'
          grp     = rgroup <$> (decimal <|> curly)
          escaped = rtext . T.singleton <$> char '$'

raw :: Parser Replace
raw = rtext <$> takeWhile1 (/= '$')
