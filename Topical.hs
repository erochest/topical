{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Char
import qualified Data.HashSet                           as S
import qualified Data.List                              as L
import           Data.Monoid
import qualified Data.Text                              as T
import qualified Data.Text.IO                           as TIO
import           System.Environment

import           Topical.Text.Tokenizer
import           Topical.Text.Tokenizer.TextTiling

import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy


main :: IO ()
main = do
    (stopfile:files) <- getArgs

    stoplist <- S.fromList . tokenize <$> TIO.readFile stopfile

    forM_ files $ \filename -> do
        putStrLn filename

        tokens <-  filter (not . (`S.member` stoplist)) . tokenize
               <$> TIO.readFile filename
        putStrLn $ "Token count = " ++ show (length tokens)

        let tiles = map _seqItems
                  $ textTilingTokenizer 20 6 blockComparison (Smoothing 1 2) tokens
        putStrLn $ "Tile count  = " ++ show (length tiles)

        forM_ tiles $ TIO.putStr
                    . (<> "\n\n")
                    . T.intercalate "\n\n"
                    . map ((<> ".") . T.intercalate " " . _seqItems)

{-
 -         chart tiles
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

chart :: [SeqScore T.Text (Double, Double)] -> IO ()
chart seqs = toFile def "tiles.png" $ do
    layout_title .= "TextTiling scores"
    -- layout_y_axis . laxis_override .= axisGridHide
    -- plot $ line "raw"   [map (fst . _seqSpan *** fst) seqs]
    plot $ line "score" [map (fst . _seqSpan *** snd) seqs]

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
