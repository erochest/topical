{-# LANGUAGE OverloadedStrings #-}


module Topical.Text.RegexSpec where


import           Data.Maybe
import           Data.Monoid
import qualified Data.Text          as T
import           Data.Text.ICU
import qualified Data.Text.ICU      as ICU

import           Test.Hspec

import           Topical.Text.Regex


spec :: Spec
spec = describe "Topical.Text.Regex" $ do
    let the   = "the"
        them  = "the(m)"
        xyzzy = "xyzzy"

    describe "replace" $ do
        it "should replace one instance of a regular expression in a string." $
            replace the "a" "the boy and the girl" `shouldBe`
                "a boy and the girl"
        it "should return the original string if there were no matches." $
            replace xyzzy "!!!" "the boy and the girl" `shouldBe`
                "the boy and the girl"

    describe "replaceAll" $ do
        it "should replace all instances of a regular expression in a string." $
            replaceAll the "a" "the boy and the girl" `shouldBe`
                "a boy and a girl"
        it "should return the original string if there were no matches." $
            replaceAll xyzzy "!!!" "the boy and the girl" `shouldBe`
                "the boy and the girl"

    describe "rgroup" $ do
        it "should pull out a captured group." $
            replace them (rgroup 1) "them boys and those girls" `shouldBe`
                "m boys and those girls"
        it "should combine with other replacements." $
            replace them ("<" <> rgroup 1 <> ">") "them boys and those girls" `shouldBe`
                "<m> boys and those girls"

    describe "parseReplace" $ do
        it "should handle raw strings." $
            replace them (parseReplace "those") "them boys and those girls" `shouldBe`
                "those boys and those girls"
        it "should handle $N groups." $
            replace them (parseReplace "$1") "them boys and those girls" `shouldBe`
                "m boys and those girls"
        it "should handle ${N} groups." $
            replace them (parseReplace "${1}") "them boys and those girls" `shouldBe`
                "m boys and those girls"
        it "should handle interpolating $N groups and raw strings." $
            replace them (parseReplace "l $1 n") "them boys and those girls" `shouldBe`
                "l m n boys and those girls"
        it "should handle interpolating ${N} groups and raw strings." $
            replace them (parseReplace "0${1}2") "them boys and those girls" `shouldBe`
                "0m2 boys and those girls"
        it "should interpolate the whole match for $0." $
            replace them (parseReplace "<$0>") "them boys and those girls" `shouldBe`
                "<them> boys and those girls"
        it "should interpolate an empty string for $N, where N > the number of groups." $
            replace them (parseReplace "<$10>") "them boys and those girls" `shouldBe`
                "<> boys and those girls"
