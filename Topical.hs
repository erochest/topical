{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Char
import qualified Data.HashSet                      as S
import           Data.Monoid
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as TIO
import           System.Environment

import           Topical.Text.Tokenizer
import           Topical.Text.Tokenizer.TextTiling


main :: IO ()
main = do
    (stopfile:files) <- getArgs

    stoplist <- S.fromList . tokenize <$> TIO.readFile stopfile

    forM_ files $ \filename -> do
        putStrLn filename

        tokens <- filter (not . (`S.member` stoplist)) . tokenize <$> TIO.readFile filename
        let tiles = uncurry zip $ textTilingTokenizer 20 6 undefined blockComparison tokens

        forM_ tiles $ \((_, s1), (Sequence{..}, s2)) -> do
            let (start, end) = _seqSpan
            TIO.putStrLn $ T.intercalate "," [ tshow _seqNo
                                             , tshow start
                                             , tshow end
                                             , tshow s1
                                             , tshow s2
                                             , T.unwords _seqItems
                                             ]

        putStrLn ""

tshow :: Show a => a -> T.Text
tshow = T.pack . show

word :: Parser T.Text
word = T.pack <$> many1 (satisfy isAlphaNum)

tokenize :: T.Text -> [T.Text]
tokenize = map T.toLower . parserTokenizer word
