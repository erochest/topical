{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}


module Topical.Text.Tokenizer.TextTiling
    ( textTilingTokenizer
    , windows
    , partition
    , triples
    , frequencies
    , blockComparison
    , vocabularyIntroduction
    , Sequence(..)
    ) where


import           Control.Applicative
import           Control.Arrow
import           Control.Error
import           Control.Lens
import           Data.Foldable       hiding (concat)
import           Data.Hashable
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import qualified Data.List           as L
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text           as T
import           Data.Traversable
import qualified Data.Vector         as V
import           Statistics.Sample

import           Topical.Text.Types


type Frequency         = Int
type TokenSeqFrequency = (SeqNumber, Frequency)

-- | "The morpholocially-analyzed token is stored in a table along with
-- a record of the token-windowSeq number it occurred in, and the number of
-- times it appeared in the token-sequence."
type TokenTable a      = M.HashMap a [TokenSeqFrequency]

type SeqNumber = Int

-- | A record is also kept of the locations of the paragraph breaks within
-- the text. Stop words contribute to the computation of the size of the
-- token windowSeq, but not to the computation of the similarity between
-- blocks of text.
type SeqSpan = (Int, Int)

type FrequencyTable a = M.HashMap a Frequency

data Sequence a b = Sequence
                  { _seqNo    :: !SeqNumber
                  , _seqSpan  :: !SeqSpan
                  , _seqItems :: ![a]
                  , _seqFreqs :: !(FrequencyTable b)
                  } deriving (Show, Eq)
makeLenses ''Sequence

type TokenSequence a = Sequence a a
type BlockSequence a = Sequence (TokenSequence a) a

type SeqScore a = (TokenSequence a, Double)

data SmoothingParams  = Smoothing Int    -- ^ Number of rounds of smoothing
                                  Int    -- ^ Smoothing window size

type ScoringFunction a =
    TokenSeqSize -> BlockSize -> [TokenSequence a] -> [SeqScore a]
type CountFunction a b = (Hashable b, Eq b) => [a] -> FrequencyTable b

type TokenSeqSize = Int
type BlockSize    = Int

-- | The basic processing flow:
-- boundary identification . lexical score determination . tokenization
textTilingTokenizer :: (Hashable a, Eq a)
                    => TokenSeqSize
                    -- ^ The token windowSeq size parameter (/w/). 20 is often
                    -- a good default value.
                    -> BlockSize
                    -- ^ The number of token sequences to group together.
                    -- This is the average paragraph length (in token
                    -- sequences) (/blocksize/). 6 often works well.
                    -> Int
                    -- ^ The minimum number of token sequences between
                    -- boundaries.
                    -> ScoringFunction a
                    -- ^ This either uses block comparison to look at
                    -- shared vocabulary or vocabulary introduction to base
                    -- it on the number of new words are found in the
                    -- second window. Either way, larger output values
                    -- should indicate a more likely change of topic.
                    -> [a]
                    -- ^ The input sequence. If this is text, this should
                    -- be tokenized, case-folded, and stop-words filtered
                    -- out. Also, affixes and irregular forms normalized
                    -- should be removed so that it's only the
                    -- morphological base.
                    -> ([SeqScore a], [SeqScore a])
                    -- -> [[a]]
textTilingTokenizer w k _minP scoring =
      -- _rest
      (id &&& snd . mapAccumConcat boundary (0, 0, []))
    . scoring w k
    . partitionSeq w frequencies

type BoundaryId a = (Double, Double, [Double -> SeqScore a])

boundary :: BoundaryId a -> SeqScore a -> (BoundaryId a, [SeqScore a])
boundary (leftS, lastS, pending) (ts, s)
    | lastS <= s = ((leftS', s, nextf:pending), [])
    | otherwise  = ((s, s, []), reverse (map ($ lastS) pending))
    where
        leftS'  = max leftS s
        nextf r = (ts, (leftS' - s) + (r - s))

-- | This converts a two-item list into a tuple pair.
toPair :: [a] -> Maybe (a, a)
toPair [a, b] = Just (a, b)
toPair _      = Nothing

toTriples :: [a] -> Maybe (a, a, a)
toTriples [a, b, c] = Just (a, b, c)
toTriples _         = Nothing

mapAccumConcat :: (a -> b -> (a, [c])) -> a -> [b] -> (a, [c])
mapAccumConcat f s = fmap concat . mapAccumL f s

-- TODO: I think this should be a comonad, but right now I just want to get
-- it written.
mapAccumContext :: (s -> [x] -> x -> [x] -> (s, [a])) -> s -> [x] -> (s, [a])
mapAccumContext f s xs = go s [] xs
    where
        go s ls []     = (s, [])
        go s ls (x:xs) = let (s',  as)  = f s ls x xs
                             (s'', ass) = go s' (x:ls) xs
                         in  (s'', as ++ ass)

-- | The block comparison scoring function. This looks at words in common
-- between two windows.
-- TODO: I think we're loosing `bsize` token seqeuences off the front.
blockComparison :: (Hashable a, Eq a) => ScoringFunction a
blockComparison _ bsize = map (uncurry blockCompare)
                        . mapMaybe toPair
                        . windows 2
                        . windowSeq bsize mergeFrequencies

blockCompare :: (Hashable a, Eq a)
             => BlockSequence a -> BlockSequence a -> SeqScore a
blockCompare (Sequence _ _ _ b1) (Sequence _ _ (ts:_) b2) =
    (ts,) . final . foldMap step $ terms b1 `S.union` terms b2
    where
        topf w1 w2     = w1 * w2
        bottoml w1 _w2 = w1 * w1
        bottomr _w1 w2 = w2 * w2
        getFreqs t     = (M.lookupDefault 0 t b1, M.lookupDefault 0 t b2)
        step           = (   Sum . uncurry topf
                         &&& Sum . uncurry bottoml
                         &&& Sum . uncurry bottomr
                         ) . getFreqs
        final (Sum t, (Sum bl, Sum br)) =
            fromIntegral t / sqrt (fromIntegral $ bl * br)

-- | This returns the terms in a frequency table.
terms :: (Hashable a, Eq a) => FrequencyTable a -> S.HashSet a
terms = S.fromList . M.keys

-- | The vocabulary introduction scoring function. This looks at how many
-- words are introduced in the second window.
-- TODO: What happens to the first item from the first pair in `collapse`?
-- TODO: Are we looking a token sequence off the front?
vocabularyIntroduction :: (Hashable a, Eq a) => ScoringFunction a
vocabularyIntroduction tsSize _ = snd
                                . mapAccumL accum S.empty
                                . map (uncurry collapse)
                                . mapMaybe toPair
                                . windows 2
                                . map (id &&& (terms . _seqFreqs))
    where
        blockSize :: Double
        blockSize = fromIntegral tsSize * 2.0
        collapse (_, t1) (ts, t2) = (ts, t1 `S.union` t2)
        accum s (ts, block) =
            let unseen = S.size $ block `S.difference` s
                s'     = s `S.union` block
            in  (s', (ts, (fromIntegral unseen) / blockSize))

-- | This extends @windows@ to create @Sequence@ instances.
windowSeq :: (Eq b, Hashable b)
          => Int -> CountFunction a b -> [a] -> [Sequence a b]
windowSeq k counter = toSeq (windows k) counter

-- | This extends @partition@ to create @Sequence@ data.
partitionSeq :: (Eq b, Hashable b)
             => Int -> CountFunction a b -> [a] -> [Sequence a b]
partitionSeq k counter = toSeq (partition k) counter

toSeq :: (Eq b, Hashable b)
      => ([(Int, a)] -> [[(Int, a)]])
      -> CountFunction a b -> [a] -> [Sequence a b]
toSeq breaker counter =
    mapMaybe (uncurry toSeq') . zip [0..] . breaker . zip [0..]
    where
        toSeq' n spanned =
            let items = map snd spanned
            in  Sequence n <$> getSpan spanned
                           <*> pure items
                           <*> pure (counter items)
        getSpan []          = Nothing
        getSpan [(s, _)]    = Just (s, s)
        getSpan ((s, _):xs) = (s,) . fst <$> lastZ xs

-- | This takes an input windowSeq and divides it into overlapping
-- subsequences of a given size.
--
-- >>> windows 2 [1, 2, 3, 4, 5]
-- [[1, 2], [2, 3], [3, 4], [4, 5], [5]]
windows :: Int      -- ^ The size of the sliding window.
        -> [a]      -- ^ The input sequence.
        -> [[a]]    -- ^ The output windowSeq of windows.
windows _ []        = []
windows k xs@(_:ys) = L.take k xs : windows k ys

-- | This takes an input windowSeq and divides it into non-overlapping
-- partitions of a given size.
--
-- >> partition 2 [1, 2, 3, 4, 5]
-- [[1, 2], [3, 4], [5]]
partition :: Int    -- ^ The size of the non-overlapping partitions.
          -> [a]    -- ^ The input windowSeq to partition.
          -> [[a]]  -- ^ The non-overlapping partitions.
partition _ [] = []
partition k xs = uncurry (:) . fmap (partition k) $ L.splitAt k xs

-- | This takes an input windowSeq and divides it into overlapping chains of
-- three.
--
-- >>> triples [1, 2, 3, 4, 5]
-- [(1, 2, 3), (2, 3, 4), (3, 4, 5)]
triples :: [a] -> [(a, a, a)]
triples = mapMaybe triples' . L.tails
    where
        triples' (a:b:c:_) = Just (a, b, c)
        triples' _         = Nothing

frequencies :: CountFunction a a
frequencies xs = M.fromListWith (+) . zip xs $ L.repeat 1

mergeFrequencies :: CountFunction (Sequence a b) b
mergeFrequencies = foldMap _seqFreqs

cosSimilarity :: (Hashable a, Eq a)
              => FrequencyTable a -> FrequencyTable a -> Double
cosSimilarity b1 b2 =
    finish . foldMap step $ terms b1 `S.union` terms b2
    where step k = let w1 = M.lookupDefault 0 k b1
                       w2 = M.lookupDefault 0 k b2
                    in (Sum (w1 * w2), Sum (w1 * w1), Sum (w2 * w2))
          finish (Sum a, Sum b, Sum c) =
              fromIntegral a / sqrt (fromIntegral b * fromIntegral c)

-- | This calculates all boundaries as being places where the depth score
-- is greater than the function below (mean minus half the standard
-- deviation).
--
-- If I need better performance, I can use http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Online_algorithm
boundaries :: [Double]     -- ^ The cutoff scores between each paragraph block.
           -> [Int]        -- ^ A list of the paragraph indexes that are boundaries.
boundaries depths = undefined $ mean depths' - (fastStdDev depths' / 2.0)
    where depths' = V.fromList depths
