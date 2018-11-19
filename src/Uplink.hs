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

post :: ToJSON a => Ctx -> String -> a -> IO B.ByteString
post ctx endpoint body = do
    req <- parseRequest endpoint
    let postReq = req { method = "POST" }
    let postReqWithContentType = addRequestHeader hContentType (C8.pack "application/json") postReq
    resp <- httpLBSFunc ctx $ withAuth ctx . withReqBody body $ postReqWithContentType
    return $ getResponseBody resp
