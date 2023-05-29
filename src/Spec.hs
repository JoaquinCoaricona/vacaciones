{-# LANGUAGE InstanceSigs #-}
module Spec where
import PdePreludat
import Library
import Test.Hspec
import Data.List (intersect)

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2
