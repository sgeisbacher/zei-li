module Main
where

import Test.Tasty
import Test.Tasty.Hspec

import TimeTrackingSpec

main :: IO ()
main = do
    timeExprToSecSpec <- testSpec "timeExprToSec()" testTimeExprToSec
    getOptionSpec <- testSpec "getOption()" testGetOption
    calculateStartSpec <- testSpec "calculateStart()" testCalculateStart
    defaultMain (testGroup "tests" [
            testGroup "TimeTracking" [timeExprToSecSpec, getOptionSpec, calculateStartSpec] 
        ])
