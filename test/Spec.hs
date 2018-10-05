{-# LANGUAGE DataKinds, FlexibleContexts, TemplateHaskell,
             TypeApplications #-}
module Main (main) where
import Data.Foldable (toList)
import Test.Hspec as H
import Frames
import Frames.Dsv

dsvTableTypes "Multi" "test/data/multiline.csv"

main :: IO ()
main = hspec $ do
  describe "Embedded newlines" $ do
    frame <- H.runIO $
      (inCoreAoS (readDsvTable "test/data/multiline.csv")
       :: IO (Frame Multi))
    it "Parses the file" $
      rget @X (toList frame !! 2) `shouldBe` 30
