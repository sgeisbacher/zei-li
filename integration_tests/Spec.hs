{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Context
import Activities as A
import TimeTracking as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as C8L
import Data.List
import Network.HTTP.Simple
import Network.HTTP.Conduit
import Network.HTTP.Client.Internal
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import Network.HTTP.Types.Header


ctx = Ctx {
    token = "some-token",
    endpointActivities = "https://api.timeular.com/api/v2/activities",
    endpointTimeTrackingStart = "https://api.timeular.com/api/v2/tracking/{activityId}/start",
    httpLBSFunc = httpLBS
}

httpLBSMock :: (Int, String) -> Request -> IO (Response B.ByteString)  
httpLBSMock (status, body) _ = do
    return Response { responseStatus = mkStatus status "success"
    , responseVersion = http11
    , responseHeaders = [(hContentType, "application/json;charset=UTF-8")]
    , responseBody = C8L.pack body
    , responseCookieJar = createCookieJar []
    , responseClose' = ResponseClose (return () :: IO ())
    }

createHttpLBSWithResponseFromFile :: String -> IO (Request -> IO (Response B.ByteString))
createHttpLBSWithResponseFromFile filename = do
    fcontent <- readFile $ "test-resources/" ++ filename
    return $ httpLBSMock (200, fcontent)

httpLBSDispatcherMock :: [(String, IO String)] -> Request -> IO (Response B.ByteString)
httpLBSDispatcherMock pathToFileMappings req = do
    let rpath = path req
    let mResp = Data.List.find (\(l,_) -> l == (B8.unpack rpath)) pathToFileMappings
    case mResp of
        Just (_, resp) -> do 
            ioresp <- resp
            httpLBSMock (200, ioresp) req
        Nothing -> do
            putStrLn $ "unknown request: " ++ B8.unpack rpath
            httpLBSMock (500, ("unknown request: " ++ B8.unpack rpath)) req

createHttpLBSDispatcherMock :: [(String, String)] -> IO (Request -> IO (Response B.ByteString))
createHttpLBSDispatcherMock pathToFileMappings = do
    let mappings2 = map (\(l, r) -> (l, readFile $ "test-resources/" ++ r)) pathToFileMappings
    return $ httpLBSDispatcherMock mappings2

main :: IO ()
main = hspec $ do
    listActivitiesSpec
    startTimeTrackingSpec

listActivitiesSpec =
    describe "list Activities" $ do
        it "lists all when no prefix is given" $ do
            f <- createHttpLBSWithResponseFromFile "respListActivities-01.json"
            let ctxMock = ctx { httpLBSFunc = f }
            A.list ctxMock [] `shouldReturn` "phone-calls\nemailing\n"
        it "performs prefix search when prefix is given" $ do
            f <- createHttpLBSWithResponseFromFile "respListActivities-01.json"
            let ctxMock = ctx { httpLBSFunc = f }
            A.list ctxMock ["e"] `shouldReturn` "emailing\n"
            A.list ctxMock ["p"] `shouldReturn` "phone-calls\n"
        -- TODO check for no result prefix search

startTimeTrackingSpec =
    describe "start TimeTracking" $ do
        it "starts tracking for given activity" $ do
            let pathToFileMappings = [ ("/api/v2/activities", "respListActivities-01.json")
                                     , ("/api/v2/tracking/18/start", "respStartTracking-01.json") ]
            f <- createHttpLBSDispatcherMock pathToFileMappings
            let ctxMock = ctx { httpLBSFunc = f }
            T.start ctxMock ["emailing"] `shouldReturn` "started emailing"
