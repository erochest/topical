{-# LANGUAGE TupleSections #-}


module Topical.VSpace where


import           Control.Lens
import           Data.Foldable        hiding (toList)
import           Data.Hashable
import qualified Data.HashMap.Strict  as M
import qualified Data.List            as L
import           Data.Ord
import           Data.Traversable     ()

import           Topical.VSpace.Types


addToken :: (Eq t, Hashable t) => t -> VSpace s t -> (Int, VSpace s t)
addToken t vs =
    maybe (insertToken t vs) (, vs) . M.lookup t $ vs ^. vsTokenMap

insertToken :: (Eq t, Hashable t) => t -> VSpace s t -> (Int, VSpace s t)
insertToken t vs = let i = M.size $ vs ^. vsTokenMap
                   in  (i, vs & over vsTokenMap (M.insert t i)
                              & over vsIndexMap (M.insert i t))

addTokens :: (Foldable f, Eq t, Hashable t) => VSpace s t -> f t -> VSpace s t
addTokens = foldl' addToken'
    where
      addToken' vs t = snd $ addToken t vs

toList :: VSpace s t -> [(t, Int)]
toList = M.toList . view vsTokenMap

fromList :: (Foldable f, Eq t, Hashable t) => f t -> VSpace s t
fromList = addTokens newVSpace

toIndices :: (Traversable ts, Eq t, Hashable t)
             => VSpace s t -> ts t -> ts (Maybe Int)
toIndices vs = fmap (`M.lookup` tm)
    where
      tm = vs ^. vsTokenMap

fromIndices :: Traversable ts => VSpace s t -> ts Int -> ts (Maybe t)
fromIndices vs = fmap (`M.lookup` im)
    where
      im = vs ^. vsIndexMap

getTokenIndices :: VSpace s t -> [(t, Int)]
getTokenIndices = L.sortBy (comparing snd) . toList
