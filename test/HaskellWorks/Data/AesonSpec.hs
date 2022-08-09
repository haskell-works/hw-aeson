{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module HaskellWorks.Data.AesonSpec
  ( spec
  ) where

import Data.Aeson
import HaskellWorks.Data.Aeson
import Test.Hspec

import qualified HaskellWorks.Data.Aeson.Compat.Map as JM

{- HLINT ignore "Redundant do"        -}

spec :: Spec
spec = describe "HaskellWorks.Data.Aeson" $ do
  it "objectWithoutNulls should stould strip nulls" $ do
    let actual = objectWithoutNulls
          [ "one"    .= (1 :: Int)
          , "two"    .= Just (2 :: Int)
          , "three"  .= (Nothing :: Maybe Int)
          ]
    let expected = object
          [ "one"    .= (1 :: Int)
          , "two"    .= (2 :: Int)
          ]
    actual `shouldBe` expected
  it "Re-exports complete" $ do
    let _ = JM.toList
    let _ = JM.toMap
    let _ = JM.foldlWithKey
    let _ = JM.fromHashMapText
    let _ = JM.lookup @Int
    let _ = JM.toHashMapText
    let _ = JM.toList
    let _ = JM.toMap

    True
