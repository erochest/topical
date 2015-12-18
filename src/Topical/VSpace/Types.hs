{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}


module Topical.VSpace.Types
    ( VSpace
    , vsTokenMap
    , vsIndexMap
    , newVSpace
    ) where


import           Control.Lens
import           Data.Data
import           Data.Foldable       ()
import qualified Data.HashMap.Strict as M
import           GHC.Generics


data VSpace s t
    = VS
    { _vsTokenMap :: M.HashMap t Int
    , _vsIndexMap :: M.HashMap Int t
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''VSpace)

instance Foldable (VSpace s) where
    foldr f s = foldr f s . M.keys . _vsTokenMap

newVSpace :: VSpace s t
newVSpace = VS M.empty M.empty
