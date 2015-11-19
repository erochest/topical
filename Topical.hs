{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


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


-- training:
-- 1. Determine vocabulary;
-- 2. Initialize (OnlineLDA.__init__)
-- 3. train on a batch of documents
--    a. `update_lambda`
--    b. returns (gamma, bound)
-- 4. estimate held-out perplexity
--    a. `parse_doc_list`
--    b. batch of documents,
--    c. vocabulary,
--    d. returns (word ids, word counts)
-- 5. track/save
--    a. lambda, the parameters to the variational distributions over topics;
--    b. gamma, the parameters to the variational distributions over topic
--       weights for the articles analyzed in the last iteration;

-- dirichlet_expectation
-- alpha -> E[log(Dir(alpha))]

-- parse_doc_list
-- [Document] -> Vocab -> [[(WordId, WordCount)]]

-- OnlineLDA from Hoffman, et al., 2010

-- OnlineLDA.__init__
--    a. vocabulary,
--    b. topic count (K),
--    c. number of documents in population (D),
--    d. alpha (1/K),
--    e. eta (1/K),
--    f. tau0 (1024),
--    g. kappa (0.7);
-- _vocab :: Map Token WordId
-- _W = size _vocab
-- _tau0 = tau0 + 1
-- _updatect = 0
-- _lambda = random matrix
-- _Elogbeta = dirichlet_expectation(_lambda)
-- _expElogbeta = exp(_Elogbeta)

-- OnlineLDA.do_e_step
-- [Document] -> (Map DocumentId GammaMatrix, ExpectedSufficientStats)
-- estimates the parameters gamma controlling the variational distribution
-- over the topic weights for each document input

-- OnlineLDA.update_lambda
-- [Document] -> (Map DocumentId GammaMatrix, OldLambda)
-- Performs E steps on mini-batches and updates variational parameter matrix
-- lambda

-- OnlineLDA.approx_bound
-- [Document] -> Map DocumentId GammaMatrix -> Double
-- Estimates the variational bound over all documents using the input subset.
-- Output is noisy, but can be used to assess convergence.

-- printtopics
-- 1. read vocabulary;
-- 2. read lambdas;
-- 3. lambda_word = vector of lambdas over topics for a word;
-- 4. lambdak = lambda_word / sum(lambda_word);
-- 5. sort and print

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
