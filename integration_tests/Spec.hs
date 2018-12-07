{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import qualified Mocks as M
import Network.HTTP.Simple (httpLBS)
import Data.Time.Clock (getCurrentTime)
import Context
import Activities as A
import TimeTracking as T
import Data.List

ctx = Ctx {
    token = "some-token",
    endpointActivities = "https://api.timeular.com/api/v2/activities",
    endpointTimeTrackingStart = "https://api.timeular.com/api/v2/tracking/{activityId}/start",
    httpLBSFunc = httpLBS,
    getCurrentTimeFunc = getCurrentTime
}

main :: IO ()
main = hspec $ do
    listActivitiesSpec
    startTimeTrackingSpec

listActivitiesSpec =
    describe "list Activities" $ do
        it "lists all when no prefix is given" $ do
            let ctxMock = ctx { httpLBSFunc = M.httpLBSMock $ M.fileResp "respListActivities-01.json" }
            A.list ctxMock [] `shouldReturn` "phone-calls\nemailing\n"
        it "performs prefix search when prefix is given" $ do
            let ctxMock = ctx { httpLBSFunc = M.httpLBSMock $ M.fileResp "respListActivities-01.json" }
            A.list ctxMock ["e"] `shouldReturn` "emailing\n"
            A.list ctxMock ["emailing"] `shouldReturn` "emailing\n"
            A.list ctxMock ["p"] `shouldReturn` "phone-calls\n"
        -- TODO check for no result prefix search

startTimeTrackingSpec =
    describe "start TimeTracking" $ do
        it "starts tracking for given activity" $ do
            let pathToFileMappings = [ ("/api/v2/activities", M.fileResp "respListActivities-01.json")
                                     , ("/api/v2/tracking/18/start", M.fileResp "respStartTracking-01.json") ]
            let ctxMock = ctx { httpLBSFunc = M.httpLBSDispatcherMock pathToFileMappings }
            T.start ctxMock ["emailing"] `shouldReturn` "started emailing"
