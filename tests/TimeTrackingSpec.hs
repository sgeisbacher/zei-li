module TimeTrackingSpec
where

import Test.Tasty
import Test.Tasty.Hspec
import Data.Time.Clock.POSIX

import TimeTracking

testTimeExprToSec :: Spec 
testTimeExprToSec = do
    it "2h == 7200 sec" $
        timeExprToSec "2h" `shouldBe` 7200
    it "2h10m15s == 7815 sec" $
        timeExprToSec "2h10m15s" `shouldBe` 7815
    it "18s == 18 sec" $
        timeExprToSec "18s" `shouldBe` 18
    it "4m == 240 sec" $
        timeExprToSec "4m" `shouldBe` 240
    it "2h2s == 7202 sec" $
        timeExprToSec "2h2s" `shouldBe` 7202

testGetOption :: Spec
testGetOption = do
    it "finds the option in the middle of the option-list" $
        (getOption "offset" ["note", "some note...", "offset", "2h10m", "someotheroption", "option-value"]) `shouldBe` Just "2h10m"
    it "finds the option at the beginning of the option-list" $
        (getOption "offset" ["offset", "4m12s", "someotheroption", "option-value"]) `shouldBe` Just "4m12s"
    it "finds the option at the end of the option-list" $
        (getOption "offset" ["someotheroption", "option-value", "offset", "15s"]) `shouldBe` Just "15s"
    it "return Nothing on empty option-name" $
        (getOption "" ["someotheroption", "option-value", "offset", "15s"]) `shouldBe` Nothing
    it "return Nothing on empty option-list" $
        (getOption "offset" []) `shouldBe` Nothing
    it "return Nothing on missing option-value" $
        (getOption "offset" ["note", "some note", "offset"]) `shouldBe` Nothing
    
testCalculateStart :: Spec
testCalculateStart = do
    it "correctly formats date-string" $ 
        (calculateStart (posixSecondsToUTCTime 1514768461) 0) `shouldBe` "2018-01-01T01:01:01.000"
    it "correctly subtracts offset" $ 
        (calculateStart (posixSecondsToUTCTime 1514768461) 62) `shouldBe` "2018-01-01T00:59:59.000"
