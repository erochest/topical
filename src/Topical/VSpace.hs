{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}


module Topical.VSpace where


import           Control.Lens
import           Data.Foldable hiding (toList)
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as M
import           Data.Hashable
import qualified Data.List as L
import           Data.Maybe
import           Data.Ord
import           Data.Traversable ()
import           Data.Tuple
import           Data.Vector.Generic ((!?), (//))
import qualified Data.Vector.Generic as V

import           Topical.VSpace.Types


addToken :: (Eq t, Hashable t) => t -> VSpaceModel s t -> (Int, VSpaceModel s t)
addToken t vsm =
    maybe (insertToken t vsm) (, vsm) . M.lookup t $ vsm ^. vsmTokenMap

insertToken :: (Eq t, Hashable t)
               => t -> VSpaceModel s t -> (Int, VSpaceModel s t)
insertToken t vsm = let i = M.size $ vsm ^. vsmTokenMap
                in  (i, vsm & over vsmTokenMap (M.insert t i)
                            & over vsmIndexMap (M.insert i t))

addTokens :: (Foldable f, Eq t, Hashable t)
             => VSpaceModel s t -> f t -> VSpaceModel s t
addTokens = foldl' addToken'
    where
      addToken' vsm t = snd $ addToken t vsm

toList :: VSpaceModel s t -> [(t, Int)]
toList = M.toList . view vsmTokenMap

fromList :: (Foldable f, Eq t, Hashable t) => f t -> VSpaceModel s t
fromList = addTokens newVSpaceModel

getTokenIndex :: (Eq t, Hashable t) => VSpaceModel s t -> t -> Maybe Int
getTokenIndex vsm t = M.lookup t $ vsm ^. vsmTokenMap

getIndexToken :: VSpaceModel s t -> Int -> Maybe t
getIndexToken vsm t = M.lookup t $ vsm ^. vsmIndexMap

toIndices :: (Traversable ts, Eq t, Hashable t)
            => VSpaceModel s t -> ts t -> ts (Maybe Int)
toIndices vsm = fmap (`M.lookup` tm)
    where
      tm = vsm ^. vsmTokenMap

fromIndices :: Traversable ts => VSpaceModel s t -> ts Int -> ts (Maybe t)
fromIndices vsm = fmap (`M.lookup` im)
    where
      im = vsm ^. vsmIndexMap

getTokenIndices :: VSpaceModel s t -> [(t, Int)]
getTokenIndices = L.sortBy (comparing snd) . toList

toVector :: (Traversable ts, Eq t, Hashable t, V.Vector v x)
            => VSpaceModel s t -> x -> ts (t, x) -> VSpace s v x
toVector vsm d =
    VS . (V.replicate (M.size tm) d //)
           . mapMaybe (fmap swap . sequenceA . fmap (`M.lookup` tm) . swap)
           . F.toList
    where
      tm = vsm ^. vsmTokenMap

frequencies :: (Traversable ts, Eq t, Hashable t, V.Vector v Int)
               => VSpaceModel s t -> ts t -> VSpace s v Int
frequencies vsm = toVector vsm 0 . M.toList . foldl' insert M.empty
    where
      insert m x = M.insertWith (+) x 1 m

vectorValue :: (Eq t, Hashable t, V.Vector v x)
               => VSpaceModel s t -> VSpace s v x -> t -> Maybe x
vectorValue vsm vs t = ((vs ^. vsVector) !?) =<< getTokenIndex vsm t
