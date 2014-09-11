{-# LANGUAGE OverloadedStrings #-}


module Topical.Text.RegexSpec where


import           Data.Maybe
import           Data.Monoid
import qualified Data.Text          as T
import           Data.Text.ICU
import qualified Data.Text.ICU      as ICU

import           Test.Hspec

import           Topical.Text.Regex


findReplace :: Regex -> Replace -> T.Text -> T.Text
findReplace re repl t = fromMaybe t (return . replace repl =<< find re t)

spec :: Spec
spec = describe "Topical.Text.Regex" $ do
    let the  = "the"
        them = "the(m)"
    describe "replace" $
        it "should replace one instance of a regular expression in a string." $
            findReplace the "a" "the boy and the girl" `shouldBe`
                "a boy and the girl"

    describe "replaceAll" $
        it "should replace all instances of a regular expression in a string." $
            replaceAll "a" (findAll the "the boy and the girl") `shouldBe`
                "a boy and a girl"

    describe "rgroup" $ do
        it "should pull out a captured group." $
            findReplace them (rgroup 1) "them boys and those girls" `shouldBe`
                "m boys and those girls"
        it "should combine with other replacements." $
            findReplace them ("<" <> rgroup 1 <> ">") "them boys and those girls" `shouldBe`
                "<m> boys and those girls"
