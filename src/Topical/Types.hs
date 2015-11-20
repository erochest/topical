{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}


module Topical.Types where


import           Data.Data
import           GHC.Generics


data Actions
    = LoadCorpus { loadInput        :: !FilePath
                 , loadStopWordFile :: !(Maybe FilePath)
                 , loadOutput       :: !FilePath
                 }
    deriving (Show, Eq, Data, Typeable, Generic)
