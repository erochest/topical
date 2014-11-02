{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Char
import qualified Data.HashSet                      as S
import qualified Data.List                         as L
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
        putStrLn $ "Token count = " ++ show (length tokens)

{-
 -         let tiles = textTilingTokenizer 20 6 3 blockComparison tokens
 -         putStrLn $ "Tile count  = " ++ show (length tiles)
 -
 -         forM_ tiles $ \(Sequence{..}, (raw, score)) ->
 -             putStrLn $ L.intercalate "," [ show _seqNo
 -                                          , show (fst _seqSpan)
 -                                          , show (snd _seqSpan)
 -                                          , show raw
 -                                          , show score
 -                                          , unwords (map T.unpack _seqItems)
 -                                          ]
 -}

{-
 - toParagraphs :: Int
 -              -> Tree (SeqScore T.Text (Double, Double))
 -              -> [[SeqScore T.Text (Double, Double)]]
 - toParagraphs 0 tree = [L.sortBy (comparing (_seqNo . fst)) $ flatten tree]
 - toParagraphs n (Node root forest) = [sortp pre, root : sortp post]
 -     where
 -         rootNo      = root ^. _1 . seqNo
 -         (pre, post) = L.break ((< rootNo) . _seqNo . fst . head . head)
 -                     $ map (toParagraphs (n - 1)) forest
 -         sortp       = L.sortBy (comparing (_seqNo . fst . _))
 -}

showp :: [SeqScore T.Text (Double, Double)] -> String
showp = L.intercalate ". " . map (unwords . map T.unpack . _seqItems . fst)

tshow :: Show a => a -> T.Text
tshow = T.pack . show

word :: Parser T.Text
word = T.pack <$> many1 (satisfy isAlphaNum)

tokenize :: T.Text -> [T.Text]
tokenize = map T.toLower . parserTokenizer word
