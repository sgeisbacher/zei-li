{-# LANGUAGE OverloadedStrings #-}
module Mocks 
( httpLBSMock
, httpLBSDispatcherMock
, textResp
, fileResp
, HttpMockConfig(..))
where

import Data.List
import Network.HTTP.Simple
import Network.HTTP.Conduit
import Network.HTTP.Client.Internal
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import Network.HTTP.Types.Header
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as C8L

data HttpMockConfig = HttpMockConfig {
    respBody :: IO String,
    respStatusCode :: Int
}

httpLBSMock :: HttpMockConfig -> Request -> IO (Response B.ByteString)  
httpLBSMock cfg _ = do
    respBodyContents <- respBody cfg
    return Response { responseStatus = mkStatus (respStatusCode cfg) "success"
    , responseVersion = http11
    , responseHeaders = [(hContentType, "application/json;charset=UTF-8")]
    , responseBody = C8L.pack respBodyContents
    , responseCookieJar = createCookieJar []
    , responseClose' = ResponseClose (return () :: IO ())
    }

httpLBSDispatcherMock :: [(String, HttpMockConfig)] -> Request -> IO (Response B.ByteString)
httpLBSDispatcherMock pathToCfgMappings req = do
    let reqPath = path req
    let maybeCfg = Data.List.find (\(l,_) -> l == (B8.unpack reqPath)) pathToCfgMappings
    case maybeCfg of
        Just (_, cfg) -> do 
            httpLBSMock cfg req
        Nothing -> do
            putStrLn $ "unknown request: " ++ B8.unpack reqPath
            httpLBSMock (withFailure 500 $ textResp $ "unknown request: " ++ B8.unpack reqPath) req

readResource :: String -> IO String
readResource fileName = readFile $ "test-resources/" ++ fileName

textResp :: String -> HttpMockConfig
textResp text = HttpMockConfig { 
    respBody = return text,
    respStatusCode = 200
}

withFailure :: Int -> HttpMockConfig -> HttpMockConfig
withFailure respStatusCode cfg = cfg { respStatusCode = respStatusCode }

fileResp :: String -> HttpMockConfig
fileResp fileName = HttpMockConfig { 
    respBody = readResource fileName,
    respStatusCode = 200
}
