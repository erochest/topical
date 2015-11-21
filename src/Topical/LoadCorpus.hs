{-# LANGUAGE OverloadedStrings #-}


module Topical.LoadCorpus where


import           Control.Error
import           Control.Monad
import qualified Data.HashSet          as S
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           System.Directory
import           System.FilePath

import           Taygeta.ICU.Tokenizer
import           Taygeta.Types


loadCorpus :: FilePath -> Maybe FilePath -> FilePath -> Script ()
loadCorpus input stopWords _output = do
  stopWords <- maybe (return S.empty) loadStopWords stopWords
  docs <-  mapM (fmap (filter (not . (`S.member` stopWords))) . tokenizeFile)
       =<< walkDirectory input
  -- open the output file
    -- Walk over docs
            -- convert each to a vector-space model
            -- write the data to the file
  undefined

tokenizeFile :: FilePath -> Script [PlainToken]
tokenizeFile = fmap tokenize . scriptIO . TIO.readFile

loadStopWords :: FilePath -> Script (S.HashSet PlainToken)
loadStopWords = fmap S.fromList . tokenizeFile

tokenize :: PlainTokenizer
tokenize = fmap T.toLower . regexTokenizer "[\\p{L}\\p{N}_]+"

walkDirectory :: FilePath -> Script [FilePath]
walkDirectory = walk <=< scriptIO . makeAbsolute
    where
      walk :: FilePath -> Script [FilePath]
      walk root = do
        children <- fmap (root </>) . filter (not . isHidden)
                    <$> scriptIO (getDirectoryContents root)
        (dirs, files) <- partitionM (scriptIO . doesDirectoryExist) children
        (files ++) . concat <$> mapM walk dirs

      isHidden []      = True
      isHidden ('.':_) = True
      isHidden _       = False

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM f (x:xs) = do
  result <- f x
  (trues, falses) <- partitionM f xs
  return $ if result
           then (x:trues, falses)
           else (trues, x:falses)
