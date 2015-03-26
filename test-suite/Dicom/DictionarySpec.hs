module Dicom.DictionarySpec (spec) where

import Dicom.Dictionary

import Test.Hspec

spec :: Spec
spec =
    describe "main" $ do
        it "returns the unit" $
            main `shouldReturn` ()
