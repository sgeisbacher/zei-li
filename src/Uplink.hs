{-# LANGUAGE OverloadedStrings #-}

module Uplink 
( get
, post
) where

import Data.Aeson
import Network.HTTP.Simple
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as C8L
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Lazy as B
import Data.List.Utils

withAuth :: String -> Request -> Request 
withAuth token = addRequestHeader hAuthorization (C8.pack $ "Bearer " ++ token)

withReqBody :: ToJSON a => a -> Request -> Request 
withReqBody = setRequestBodyJSON

get :: String -> String -> IO B.ByteString
get token endpoint = do
    req <- parseRequest endpoint
    resp <- httpLBS $ withAuth token req
    return $ getResponseBody resp

post :: ToJSON a => String -> String -> a -> IO B.ByteString
post token endpoint body = do
    req <- parseRequest endpoint
    let postReq = req { method = "POST" }
    let postReqWithContentType = addRequestHeader hContentType (C8.pack "application/json") postReq
    resp <- httpLBS $ withAuth token . withReqBody body $ postReqWithContentType
    return $ getResponseBody resp