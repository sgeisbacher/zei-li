{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Context
import Activities
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C8L
import Network.HTTP.Simple
import Network.HTTP.Conduit
import Network.HTTP.Client.Internal
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import Network.HTTP.Types.Header


ctx = Ctx {
    token = "eyJhbGciOiJIUzUxMiJ9.eyJ0eXBlIjoidXNlciIsInN1YiI6IjE1MzU4In0.y85VfiOKQRU8xA8X23K7zxM0LrsVBoXw47YF29MOeTT86cAmV0Cby0rsnu3fa_IHr93QIGazN6N6rngxN7iRMQ",
    endpointActivities = "https://api.timeular.com/api/v2/activities",
    endpointTimeTrackingStart = "https://api.timeular.com/api/v2/tracking/{activityId}/start",
    httpLBSFunc = httpLBS
}

httpLBSMock :: String -> Request -> IO (Response B.ByteString)  
httpLBSMock body _ = do
    return Response { responseStatus = mkStatus 200 "success"
    , responseVersion = http11
    , responseHeaders = [(hContentType, "application/json;charset=UTF-8")]
    , responseBody = C8L.pack body
    , responseCookieJar = createCookieJar []
    , responseClose' = ResponseClose (return () :: IO ())
    }

createHttpLBSWithResponseFromFile :: String -> IO (Request -> IO (Response B.ByteString))
createHttpLBSWithResponseFromFile filename = do
    fcontent <- readFile $ "test-resources/" ++ filename
    return $ httpLBSMock fcontent

main :: IO ()
main = hspec $ do
    listActivitiesSpec

listActivitiesSpec =
    describe "list Activities" $ do
        it "simple test" $ do
            f <- createHttpLBSWithResponseFromFile "respListActivities-01.json"
            let mockCtx = ctx { httpLBSFunc = f }
            list mockCtx [] `shouldReturn` "phone-calls\nemailing\n"
