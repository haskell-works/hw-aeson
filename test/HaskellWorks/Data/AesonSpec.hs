{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.AesonSpec
  ( spec
  ) where

import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Aeson" $ do
    it "xxx" $ do
      True `shouldBe` True
