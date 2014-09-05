{-# LANGUAGE OverloadedStrings #-}


module Topical.Text.TokenizerSpec where


import           Control.Applicative
import           Data.Attoparsec.Text
import qualified Data.Attoparsec.Text   as A
import           Data.Char
import qualified Data.Text              as T

import           Test.Hspec

import           Topical.Text.Tokenizer


spec :: Spec
spec = describe "Topical.Text.Tokenizer" $ do
    describe "splitTokenizer" $ do
        it "should split a string on whitespace." $
            splitTokenizer "the red pony rides the moors" `shouldBe`
                ["the", "red", "pony", "rides", "the", "moors"]
        it "should not separate puncutation from adjoining words." $
            splitTokenizer "the red pony! it rides the moors!" `shouldBe`
                ["the", "red", "pony!", "it", "rides", "the", "moors!"]
        it "should not normalize case (or anything else)." $
            splitTokenizer "The red pony! It rides the Moors!" `shouldBe`
                ["The", "red", "pony!", "It", "rides", "the", "Moors!"]

    describe "parserTokenizer" $ do
        it "should return all instances of a given parser." $ do
            let space' = T.singleton <$> space
                rWords = T.cons <$> (space *> char 'r') <*> A.takeWhile (not . isSpace)
            parserTokenizer (string "the") "the red pony rides the moors" `shouldBe`
                ["the", "the"]
            parserTokenizer space' "the red pony rides the moors" `shouldBe`
                [" ", " ", " ", " ", " "]
            parserTokenizer space' " the red pony rides the moors " `shouldBe`
                [" ", " ", " ", " ", " ", " ", " "]
            parserTokenizer rWords "the red pony rides the moors" `shouldBe`
                ["red", "rides"]
            parserTokenizer rWords "red pony rides the moors" `shouldBe`
                ["rides"]
