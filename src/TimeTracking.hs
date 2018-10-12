{-# LANGUAGE OverloadedStrings #-}
module TimeTracking
( start
) where

import Data.Aeson
import Context
import qualified Constants as C
import Uplink
import Data.Time.Clock 
import Data.Time.Format
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString.Lazy as B
import Data.List.Utils

import qualified Activities

newtype TrackingStart = TrackingStart String
instance ToJSON TrackingStart where
    toJSON (TrackingStart timestamp) = object
        [ "startedAt" .= timestamp ]

currTimeStamp :: IO String
currTimeStamp = take 23 . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q" <$> getCurrentTime

start :: Ctx -> [String] -> IO String
start _ [] = return "activity required!"
start ctx (activityName:_) = do
    act <- Activities.findByName ctx activityName
    case act of 
        Nothing -> return "Error while starting activity (not found)"
        Just activity -> do
            let endpoint = replace "{activityId}" (Activities.id activity) C.endpointTimeTrackingStart
            trackingStart <- TrackingStart <$> currTimeStamp
            answer <- C8.unpack <$> Uplink.post (token ctx) endpoint trackingStart
            putStrLn $ "answer: " ++ answer 
            return $ "started " ++ Activities.name activity
