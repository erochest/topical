{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE TemplateHaskell    #-}


module Topical.VSpace.Types
    ( VSpaceModel
    , vsmTokenMap
    , vsmIndexMap
    , newVSpaceModel
    , VSpace(VS)
    , vsVector
    ) where


import           Control.Lens
import           Data.Data
import           Data.Foldable       ()
import qualified Data.HashMap.Strict as M
import           GHC.Generics


data VSpaceModel s t
    = VSM
    { _vsmTokenMap :: M.HashMap t Int
    , _vsmIndexMap :: M.HashMap Int t
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''VSpaceModel)

instance Foldable (VSpaceModel s) where
    foldr f s = foldr f s . M.keys . _vsmTokenMap

newVSpaceModel :: VSpaceModel s t
newVSpaceModel = VSM M.empty M.empty

data VSpace s v t
    = VS { _vsVector :: v t }
    deriving (Show, Eq, Data, Typeable, Generic, Functor, Traversable, Foldable)
$(makeLenses ''VSpace)
