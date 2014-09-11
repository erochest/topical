{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}


module Topical.Text.Regex
    ( Replace(..)
    , replace
    , replaceAll
    , rgroup
    , rtext
    , rfn
    , rtfn
    , rbuilder
    ) where


import           Control.Arrow
import           Data.Foldable
import           Data.Monoid
import           Data.String
import qualified Data.Text              as T
import           Data.Text.ICU
import qualified Data.Text.ICU          as ICU
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TB


newtype Replace = Replace { unReplace :: Match -> TB.Builder }
                  deriving (Monoid)

instance IsString Replace where
    fromString = Replace . const . TB.fromString

replace :: Replace -> Match -> T.Text
replace r m = finish (Last (Just m)) $ unReplace r m

replaceAll :: Replace -> [Match] -> T.Text
replaceAll r ms = uncurry finish $ foldMap (Last . Just &&& build r) ms
    where
        build :: Replace -> Match -> TB.Builder
        build repl m = TB.fromText (ICU.span m) <> unReplace repl m

finish :: Last Match -> TB.Builder -> T.Text
finish m b =
      TL.toStrict . TB.toLazyText . mappend b . TB.fromText . fold
    $ suffix 0 =<< getLast m

rgroup :: Int -> Replace
rgroup g = Replace $ fold . fmap TB.fromText . group g

rtext :: T.Text -> Replace
rtext = rbuilder . TB.fromText

rfn :: (Match -> TB.Builder) -> Replace
rfn = Replace

rtfn :: (Match -> T.Text) -> Replace
rtfn = Replace . (TB.fromText .)

rbuilder :: TB.Builder -> Replace
rbuilder = Replace . const
