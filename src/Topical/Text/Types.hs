module Topical.Text.Types
    ( Tokenizer
    ) where


import qualified Data.Text as T


type Tokenizer = T.Text -> [T.Text]
