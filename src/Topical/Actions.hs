{-# LANGUAGE RecordWildCards #-}


module Topical.Actions where


import           Control.Error

import           Topical.Actions.LoadCorpus
import           Topical.Types


doActions :: Actions -> Script ()
doActions LoadCorpus{..} = loadCorpus loadInput loadStopWordFile loadOutput
