{-# LANGUAGE RecordWildCards #-}


module Actions where


import           Control.Error

import           Types

import           Topical.LoadCorpus


doActions :: Actions -> Script ()
doActions LoadCorpus{..} = loadCorpus loadInput loadStopWordFile loadOutput
