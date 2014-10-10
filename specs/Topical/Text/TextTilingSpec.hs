{-# LANGUAGE OverloadedStrings #-}


module Topical.Text.TextTilingSpec where


import qualified Data.HashMap.Strict               as M

import           Test.Hspec

import           Topical.Text.Tokenizer.TextTiling


spec :: Spec
spec = describe "Topical.Text.Tokenizer.TextTiling" $ do
    describe "windows" $ do
        it "should return an empty list." $
            windows 4 [] `shouldBe` ([] :: [String])
        it "should break a list into overlapping chunks." $
            windows 2 "abcde" `shouldBe` (["ab", "bc", "cd", "de", "e"] :: [String])

    describe "partition" $ do
        it "should return an empty list." $
            partition 4 [] `shouldBe` ([] :: [String])
        it "should break a list into non-overlapping chunks." $
            partition 2 "abcde" `shouldBe` (["ab", "cd", "e"] :: [String])

    describe "triples" $ do
        it "should return an empty list." $ do
            triples []   `shouldBe` ([] :: [(Char, Char, Char)])
            triples "a"  `shouldBe` ([] :: [(Char, Char, Char)])
            triples "ab" `shouldBe` ([] :: [(Char, Char, Char)])
        it "should return overlapping triples." $
            triples "abcdef" `shouldBe` ([ ('a', 'b', 'c'), ('b', 'c', 'd')
                                         , ('c', 'd', 'e'), ('d', 'e', 'f')
                                         ] :: [(Char, Char, Char)])

    describe "frequencies" $ do
        it "should count the items in its input." $
            frequencies "abcdabd" `shouldBe`
                M.fromList [ ('a', 2), ('b', 2), ('c', 1), ('d', 2) ]
