{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}


module Types where


import           Data.Data
import           GHC.Generics


-- TODO: Specify tokenizer regex
data Actions
    = LoadCorpus { loadInput        :: !FilePath
                 , loadStopWordFile :: !(Maybe FilePath)
                 , loadOutput       :: !FilePath
                 }
    deriving (Show, Eq, Data, Typeable, Generic)
