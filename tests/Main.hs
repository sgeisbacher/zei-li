module Main
where

import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
    simpleTestSpec <- testSpec "SimpleTest" simpleTest
    defaultMain (testGroup "tests" [
            testGroup "SimpleTests" [simpleTestSpec] 
        ])

simpleTest :: Spec
simpleTest = do
    it "1 == 1" $
        1 `shouldBe` 1
    it "1 == 1" $
        1 `shouldBe` 1
