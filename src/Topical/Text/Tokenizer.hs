{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module Topical.Text.Tokenizer
    ( splitTokenizer
    , parserTokenizer
    , sexprTokenizer
    , charTokenizer
    , lineTokenizer
    , treebankTokenizer
    ) where


import           Control.Applicative
import           Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as A
import           Data.Char            (isSpace)
import           Data.Foldable        (fold)
import qualified Data.List            as L
import           Data.Monoid
import qualified Data.Text            as T
import           Data.Text.ICU

import           Topical.Text.Regex
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

treebankTokenizer :: Tokenizer
treebankTokenizer = T.words
                  . foldRepl' (stage2 ++ contractions)
                  . flip (j ' ') ' '
                  . foldRepl' stage1
    where reOpts         = [CaseInsensitive]
          fmap1 f (l, r) = (f l, r)
          rall t (re, r) = replaceAll re r t
          foldRepl t res = L.foldl' rall t $ map (fmap1 (regex reOpts)) res
          foldRepl'      = flip foldRepl
          stage1         = [ ("^\"", "``")                  -- starting quotes
                           , ("(``)", " $1 ")
                           , ("([ (\\[{])\"", "$1 `` ")
                           , ("([:,])([^\\d])", " $1 $2 ")  -- punctuation
                           , ("\\.\\.\\.", " ... ")
                           , ("[;@#$%&]", " $0 ")
                           , ("([^\\.])(\\.)([\\]\\)}>\"']*)\\s*$", "$1 $2$3")
                           , ("[?!]", " $0 ")
                           , ("([^'])' ", "$1 ' ")
                           , ("[\\]\\[\\(\\)\\{\\}\\<\\>]", " $0 ")     -- brackets
                           , ("--", " -- ")
                           ]
          stage2         = [ ("\"", " '' ")
                           , ("(\\S)('')", "$1 $2 ")
                           , ("([^' ])('[sS]|'[mM]|'[dD]|') ", "$1 $2 ")
                           , ("([^' ])('ll|'LL|'re|'RE|'ve|'VE|n't|N'T) ", "$1 $2 ")
                           ]
          contractions   = map (, " $1 $2 ") [ "\\b(can)(not)\\b"
                                             , "\\b(d)('ye)\\b"
                                             , "\\b(gim)(me)\\b"
                                             , "\\b(gon)(na)\\b"
                                             , "\\b(got)(ta)\\b"
                                             , "\\b(lem)(me)\\b"
                                             , "\\b(mor)('n)\\b"
                                             , "\\b(wan)(na)\\b"
                                             , " ('t)(is)\\b"
                                             , " ('t)(wa)\\b"
                                             , "\\b(whad)(dd)(ya)\\b"
                                             , "\\b(wha)(t)(cha)\\b"
                                             -- Commented out in the original:
                                             -- , "\\b(whad)(dd)(ya)\\b"
                                             -- , "\\b(wha)(t)(cha)\\b"
                                             ]

parseTokens :: Parser [T.Text] -> Tokenizer
parseTokens p = fold . parseOnly (p <* endOfInput)

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
