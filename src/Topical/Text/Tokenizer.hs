{-# LANGUAGE OverloadedStrings #-}


module Topical.Text.Tokenizer
    ( splitTokenizer
    , parserTokenizer
    , sexprTokenizer
    , charTokenizer
    , lineTokenizer
    ) where


import           Control.Applicative
import           Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as A
import           Data.Char            (isSpace)
import           Data.Monoid
import qualified Data.Text            as T

import           Topical.Text.Types


splitTokenizer :: Tokenizer
splitTokenizer = T.split isSpace

parserTokenizer :: Parser T.Text -> Tokenizer
parserTokenizer p = parseTokens (many (alt p anyChar) <* takeText)

sexprTokenizer :: Tokenizer
sexprTokenizer = parseTokens sexpr

charTokenizer :: Tokenizer
charTokenizer = map T.singleton . T.unpack

lineTokenizer :: Tokenizer
lineTokenizer = T.lines

parseTokens :: Parser [T.Text] -> Tokenizer
parseTokens p = either (const []) id . parseOnly (p <* endOfInput)

alt :: Parser a -> Parser b -> Parser a
alt p s = p <|> (s *> alt p s)

sexpr :: Parser [T.Text]
sexpr = sepBy word (many1 space)

word :: Parser T.Text
word =   (j <$> char '(' <*> manym (word <|> anyCharP) <*> char ')')
     <|> A.takeWhile1 isWord

anyCharP :: Parser T.Text
anyCharP = T.singleton <$> satisfy (not . isParen)

j :: Char -> T.Text -> Char -> T.Text
j a b c = a `T.cons` b `T.snoc` c

manym :: Monoid a => Parser a -> Parser a
manym p = mconcat <$> many p

isParen :: Char -> Bool
isParen '(' = True
isParen ')' = True
isParen _   = False

isWord :: Char -> Bool
isWord c | isParen c = False
         | isSpace c = False
         | otherwise = True
