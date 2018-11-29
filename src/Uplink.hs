{-# LANGUAGE OverloadedStrings #-}

module Uplink 
( get
, post
) where

import Data.Aeson (ToJSON)
import Network.HTTP.Simple (Request, addRequestHeader, getResponseBody, setRequestBodyJSON, parseRequest)
import Network.HTTP.Conduit (method, responseStatus)
import Network.HTTP.Types.Header (hAuthorization, hContentType)
import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as B

import Context

withAuth :: Ctx -> Request -> Request 
withAuth ctx = addRequestHeader hAuthorization (C8.pack $ "Bearer " ++ token ctx)

withReqBody :: ToJSON a => a -> Request -> Request 
withReqBody = setRequestBodyJSON

get :: Ctx -> String -> IO B.ByteString
get ctx endpoint = do
    req <- parseRequest endpoint
    resp <- httpLBSFunc ctx $ withAuth ctx req
    return $ getResponseBody resp

post :: ToJSON a => Ctx -> String -> a -> IO (Maybe B.ByteString)
post ctx endpoint body = do
    req <- parseRequest endpoint
    let postReq = req { method = "POST" }
    let postReqWithContentType = addRequestHeader hContentType (C8.pack "application/json") postReq
    resp <- httpLBSFunc ctx $ withAuth ctx . withReqBody body $ postReqWithContentType
    let respStatusCode = statusCode . responseStatus $ resp
    case respStatusCode of 
        200 -> return $ Just (getResponseBody resp)
        _ -> return Nothing
