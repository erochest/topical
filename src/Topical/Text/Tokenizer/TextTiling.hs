{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}


module Topical.Text.Tokenizer.TextTiling
    ( SmoothingParams(..)

    , textTilingTokenizer
    , suppressSmallBlocks
    , smooth

    , windows
    , partition
    , triples
    , frequencies
    , blockComparison
    , vocabularyIntroduction

    , Sequence(..)
    , seqNo
    , seqSpan
    , seqItems
    , seqFreqs

    , SeqScore
    ) where


import           Control.Arrow
import           Control.Error
import           Control.Lens
-- import           Data.Foldable       hiding (concat)
import           Data.Hashable
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import qualified Data.List           as L
import qualified Data.List.Split     as Split
import           Data.Monoid
import           Data.Ord
import qualified Data.Text           as T
import           Data.Traversable
-- import qualified Data.Vector         as V
-- import           Statistics.Sample

-- import           Topical.Text.Types


type Frequency         = Int
-- type TokenSeqFrequency = (SeqNumber, Frequency)

-- | "The morpholocially-analyzed token is stored in a table along with
-- a record of the token-windowSeq number it occurred in, and the number of
-- times it appeared in the token-sequence."
-- type TokenTable a      = M.HashMap a [TokenSeqFrequency]

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
                  } deriving (Eq)
makeLenses ''Sequence

instance Show (Sequence T.Text b) where
    show Sequence{..} =
        mconcat [ "Sequence #"
                , show _seqNo
                , ": "
                , T.unpack (T.unwords _seqItems)
                ]

type TokenSequence a = Sequence a a
type BlockSequence a = Sequence (TokenSequence a) a

type SeqScore a b = (TokenSequence a, b)

data SmoothingParams  = Smoothing Int    -- ^ Number of rounds of smoothing
                                  Int    -- ^ Smoothing window size

type ScoringFunction a =
    TokenSeqSize -> BlockSize -> [TokenSequence a] -> [SeqScore a Double]
type CountFunction a b = (Hashable b, Eq b) => [a] -> FrequencyTable b

type TokenSeqSize = Int
type BlockSize    = Int

-- | The basic processing flow:
-- boundary identification . lexical score determination . tokenization
textTilingTokenizer :: (Hashable a, Eq a, Show a)
                    => TokenSeqSize
                    -- ^ The token windowSeq size parameter (/w/). 20 is often
                    -- a good default value.
                    -> BlockSize
                    -- ^ The number of token sequences to group together.
                    -- This is the average paragraph length (in token
                    -- sequences) (/blocksize/). 6 often works well.
                    -> ScoringFunction a
                    -- ^ This either uses block comparison to look at
                    -- shared vocabulary or vocabulary introduction to base
                    -- it on the number of new words are found in the
                    -- second window. Either way, larger output values
                    -- should indicate a more likely change of topic.
                    -> SmoothingParams
                    -> [a]
                    -- ^ The input sequence. If this is text, this should
                    -- be tokenized, case-folded, and stop-words filtered
                    -- out. Also, affixes and irregular forms normalized
                    -- should be removed so that it's only the
                    -- morphological base.
                    -> [BlockSequence a]
textTilingTokenizer w k scoring (Smoothing sWin sIters) =
      cutOffs
    . smooth sWin sIters
    . snd
    . mapAccumConcat boundary (0, 0, [])
    . scoring w k
    . partitionSeq w frequencies

cutOffs :: [SeqScore a (Double, Double)] -> [BlockSequence a]
cutOffs = undefined

-- The first Double is the raw score. The second is the depth score
-- actually used to make the boundary determination.
-- type SeqNode a = SeqScore a (Double, Double)

-- | This takes a list of @SeqScore a (Double, Double)@, sorted by
-- likelihood of beginning a new section, and returns a tree of descending
-- likelihoods, but in sequence order.
-- hangTree :: Show a => [SeqNode a] -> Tree (SeqNode a)
-- hangTree = unfoldTree hang
--     where
--         hang :: Show a
--                 => [SeqNode a]
--                     -> (SeqNode a, (Maybe [SeqNode a], Maybe [SeqNode a]))
--         hang = undefined
--              . maximumBy (comparing (snd . snd . fst))
--              . snd
--              . L.mapAccumL packageNode []
--              . filter (not . L.null)
--              . L.tails
--
--         packageNode :: Show a
--                        => [SeqNode a]
--                            -> [SeqNode a]
--                            -> ([SeqNode a], (SeqNode a, [[SeqNode a]]))
--         packageNode ls (s:rs) = (s:ls, (s, [reverse ls, rs]))
--         packageNode ls []     = (ls,   (undefined, []))
--         -- It should never reach here, so I'll just plant a bomb.
--         -- KA-BOOM!

smooth :: Int -> Int -> [SeqScore a (Double, Double)]
       -> [SeqScore a (Double, Double)]
smooth _      0     sss = sss
smooth window iters sss =
    smooth window (pred iters) $ moveAvgBy window (_2 . _2) sss

moveAvgBy :: Int       -- ^ The number of items to offset from each side
                       -- of the center. For instance, for a window of 3,
                       -- use a value of 1.
          -> Lens' a Double -> [a] -> [a]
moveAvgBy n l xs = zipWith (set l) (moveavg n $ map (^. l) xs) xs

moveavg :: Int         -- ^ The number of items to offset from each side
                       -- of the center. For instance, for a window of 3,
                       -- use a value of 1.
        -> [Double] -> [Double]
moveavg n = go []
    where
        go _    []     = []
        go pref (x:xs) = avg (x : take n pref ++ take n xs) : go (x:pref) xs

avg :: [Double] -> Double
avg = uncurry (/) . L.foldl' accum (0, 0)
    where
        accum p x = ((+ x) *** succ) p

suppressSmallBlocks :: Int
                    -> [SeqScore a (Double, Double)]
                    -> [SeqScore a (Double, Double)]
suppressSmallBlocks minP = sortNo . removeTiny minP . sortScore
    where
        removeTiny :: Int
                   -> [SeqScore a0 (Double, Double)]
                   -> [SeqScore a0 (Double, Double)]
        removeTiny _ []     = []
        removeTiny w (x:xs) =
            x : sortScore (map (modifyClose w $ x ^. _1 . seqNo) xs)
        closeTo :: Int -> Int -> SeqScore a1 (Double, Double) -> Bool
        closeTo w seq1No seq2 = abs (seq1No - (seq2 ^. _1 . seqNo)) <= w
        modifyClose :: Int
                    -> Int
                    -> SeqScore a2 (Double, Double)
                    -> SeqScore a2 (Double, Double)
        modifyClose w seq1No seq2 = if closeTo w seq1No seq2
                                        then seq2 & _2 . _2 .~ 0
                                        else seq2

sortScore :: [SeqScore a (Double, Double)] -> [SeqScore a (Double, Double)]
sortScore = L.sortBy (comparing (Down . snd . snd))

sortNo :: [SeqScore a b] -> [SeqScore a b]
sortNo = L.sortBy (comparing (_seqNo . fst))

type BoundaryId a = (Double, Double, [Double -> SeqScore a (Double, Double)])

boundary :: BoundaryId a
         -> SeqScore a Double
         -> (BoundaryId a, [SeqScore a (Double, Double)])
boundary (leftS, lastS, pending) (ts, s)
    | lastS <= s = ((leftS', s, nextf:pending), [])
    | otherwise  = ((s, s, []), reverse (map ($ lastS) (nextf:pending)))
    where
        leftS'  = max leftS s
        nextf r = (ts, (s, (leftS' - s) + (r - s)))

-- | This converts a two-item list into a tuple pair.
toPair :: [a] -> Maybe (a, a)
toPair [a, b] = Just (a, b)
toPair _      = Nothing

-- toTriples :: [a] -> Maybe (a, a, a)
-- toTriples [a, b, c] = Just (a, b, c)
-- toTriples _         = Nothing

mapAccumConcat :: (a -> b -> (a, [c])) -> a -> [b] -> (a, [c])
mapAccumConcat f s = fmap concat . mapAccumL f s

-- TODO: I think this should be a comonad, but right now I just want to get
-- it written.
-- mapAccumContext :: (s -> [x] -> x -> [x] -> (s, [a])) -> s -> [x] -> (s, [a])
-- mapAccumContext f state = go state []
--     where
--         go s _  []     = (s, [])
--         go s ls (x:xs) = let (s',  as)  = f s ls x xs
--                              (s'', ass) = go s' (x:ls) xs
--                          in  (s'', as ++ ass)

-- | The block comparison scoring function. This looks at words in common
-- between two windows.
-- TODO: I think we're loosing `bsize` token sequences off the front.
blockComparison :: (Hashable a, Eq a) => ScoringFunction a
blockComparison _ _ []      = []
blockComparison _ bsize tss =
      uncurry appHead
    . (floatHead &&& comparePairs)
    $ windowSeq bsize mergeFrequencies tss
    where
        floatHead seqs = (,0.0) <$> seqs ^? i0 . seqItems . i0
        comparePairs   = map (uncurry blockCompare) . mapMaybe toPair . windows 2
        appHead mh xs  = maybe xs (:xs) mh
        i0             = traversed . index 0

blockCompare :: (Hashable a, Eq a)
             => BlockSequence a -> BlockSequence a -> SeqScore a Double
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
blockCompare _ _ = undefined

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
            in  (s', (ts, fromIntegral unseen / blockSize))

-- | This extends @windows@ to create @Sequence@ instances.
windowSeq :: (Eq b, Hashable b)
          => Int -> CountFunction a b -> [a] -> [Sequence a b]
windowSeq k = toSeq (windows k)

-- | This extends @partition@ to create @Sequence@ data.
partitionSeq :: (Eq b, Hashable b)
             => Int -> CountFunction a b -> [a] -> [Sequence a b]
partitionSeq k = toSeq (partition k)

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
windows k = filter (not . L.null) . map (take k) . L.tails

-- | This implementation is taken from Data.List.Split.Lens, before it was
-- removed.
chunking :: Int -- ^@n@
         -> Getting (Endo [a]) s a -> Fold s [a]
chunking s l f = coerce . traverse f . Split.chunksOf s . toListOf l
{-# INLINE chunking #-}

-- | This takes an input windowSeq and divides it into non-overlapping
-- partitions of a given size.
--
-- >> partition 2 [1, 2, 3, 4, 5]
-- [[1, 2], [3, 4], [5]]
partition :: Int    -- ^ The size of the non-overlapping partitions.
          -> [a]    -- ^ The input windowSeq to partition.
          -> [[a]]  -- ^ The non-overlapping partitions.
partition k xs = xs ^.. chunking k each

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

-- cosSimilarity :: (Hashable a, Eq a)
--               => FrequencyTable a -> FrequencyTable a -> Double
-- cosSimilarity b1 b2 =
--     finish . foldMap step $ terms b1 `S.union` terms b2
--     where step k = let w1 = M.lookupDefault 0 k b1
--                        w2 = M.lookupDefault 0 k b2
--                     in (Sum (w1 * w2), Sum (w1 * w1), Sum (w2 * w2))
--           finish (Sum a, Sum b, Sum c) =
--               fromIntegral a / sqrt (fromIntegral b * fromIntegral c)

-- | This calculates all boundaries as being places where the depth score
-- is greater than the function below (mean minus half the standard
-- deviation).
--
-- If I need better performance, I can use
-- http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Online_algorithm

-- boundaries :: [Double] -- ^ The cutoff scores between each paragraph block.
--            -> [Int]    -- ^ A list of the paragraph indexes that are boundaries.
-- boundaries depths = undefined $ mean depths' - (fastStdDev depths' / 2.0)
--     where depths' = V.fromList depths


-- Stuff from Main for this:
-- main :: IO ()
-- main = do
--     (stopfile:files) <- getArgs
--
--     stoplist <- S.fromList . tokenize <$> TIO.readFile stopfile
--
--     forM_ files $ \filename -> do
--         putStrLn filename
--
--         tokens <-  filter (not . (`S.member` stoplist)) . tokenize
--                <$> TIO.readFile filename
--         putStrLn $ "Token count = " ++ show (length tokens)
--
--         let tiles = map _seqItems
--                   $ textTilingTokenizer 20 6 blockComparison (Smoothing 1 2) tokens
--         putStrLn $ "Tile count  = " ++ show (length tiles)
--
--         forM_ tiles $ TIO.putStr
--                     . (<> "\n\n")
--                     . T.intercalate "\n\n"
--                     . map ((<> ".") . T.intercalate " " . _seqItems)
--
-- {-
--  -         chart tiles
--  -
--  -         forM_ tiles $ \(Sequence{..}, (raw, score)) ->
--  -             putStrLn $ L.intercalate "," [ show _seqNo
--  -                                          , show (fst _seqSpan)
--  -                                          , show (snd _seqSpan)
--  -                                          , show raw
--  -                                          , show score
--  -                                          , unwords (map T.unpack _seqItems)
--  -                                          ]
--  -}
--
-- chart :: [SeqScore T.Text (Double, Double)] -> IO ()
-- chart seqs = toFile def "tiles.png" $ do
--     layout_title .= "TextTiling scores"
--     -- layout_y_axis . laxis_override .= axisGridHide
--     -- plot $ line "raw"   [map (fst . _seqSpan *** fst) seqs]
--     plot $ line "score" [map (fst . _seqSpan *** snd) seqs]
--
-- {-
--  - toParagraphs :: Int
--  -              -> Tree (SeqScore T.Text (Double, Double))
--  -              -> [[SeqScore T.Text (Double, Double)]]
--  - toParagraphs 0 tree = [L.sortBy (comparing (_seqNo . fst)) $ flatten tree]
--  - toParagraphs n (Node root forest) = [sortp pre, root : sortp post]
--  -     where
--  -         rootNo      = root ^. _1 . seqNo
--  -         (pre, post) = L.break ((< rootNo) . _seqNo . fst . head . head)
--  -                     $ map (toParagraphs (n - 1)) forest
--  -         sortp       = L.sortBy (comparing (_seqNo . fst . _))
--  -}
--
-- showp :: [SeqScore T.Text (Double, Double)] -> String
-- showp = L.intercalate ". " . map (unwords . map T.unpack . _seqItems . fst)
--
-- tshow :: Show a => a -> T.Text
-- tshow = T.pack . show
--
-- word :: Parser T.Text
-- word = T.pack <$> many1 (satisfy isAlphaNum)
--
-- tokenize :: T.Text -> [T.Text]
-- tokenize = map T.toLower . parserTokenizer word
