{-# LANGUAGE OverloadedStrings #-}
module TimeTracking
( start
, timeExprToSec
, getOption
, calculateStart
) where

import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.List.Utils (replace)
import Text.Regex.Posix (getAllTextSubmatches, (=~))
import Data.Maybe (fromMaybe)

import qualified Activities
import Context
import Uplink

newtype TrackingStart = TrackingStart String
instance ToJSON TrackingStart where
    toJSON (TrackingStart timestamp) = object
        [ "startedAt" .= timestamp ]

getOption :: String -> [String] -> Maybe String
getOption "" _ = Nothing
getOption _ [] = Nothing
getOption _ [a] = Nothing
getOption option opts 
    | option `elem` opts = getOptionValueSafely . take 2 . dropWhile (/= option) $ opts
    | otherwise = Nothing
    where getOptionValueSafely [o] = Nothing
          getOptionValueSafely (_:value:_) = Just value

timeExprToSec :: String -> Integer
timeExprToSec "" = 0
timeExprToSec expr = do 
    let matches = map (\x -> if x == "" then "0" else x ) . getAllTextSubmatches $ expr =~ ("(([0-9]+)h)?(([0-9]+)m)?(([0-9]+)s)?" :: String) :: [String]
    let hours = read (matches!!2) :: Integer
    let minutes = read (matches!!4) :: Integer
    let seconds = read (matches!!6) :: Integer
    hours * 3600 + minutes * 60 + seconds

calculateStart :: UTCTime -> Integer -> String
calculateStart currTime offset = do
    let timeStamp = addUTCTime (realToFrac $ (-1) * offset) currTime
    take 23 . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.%q" $ timeStamp

start :: Ctx -> [String] -> IO String
start _ [] = return "activity required!"
start ctx (activityName:args) = do
    act <- Activities.findByName ctx activityName
    case act of 
        Nothing -> return $ "Error while starting activity: '" ++ activityName ++ "' not found"
        Just activity -> do
            let endpoint = replace "{activityId}" (Activities.id activity) (endpointTimeTrackingStart ctx)
            currTime <- getCurrentTime
            let trackingStart = TrackingStart <$> calculateStart currTime $ timeExprToSec . fromMaybe "0s" $ getOption "offset" args 
            resp <- Uplink.post ctx endpoint trackingStart
            case resp of
                Nothing -> return $ "could not start " ++ activityName
                Just respBody -> return $ "started " ++ Activities.name activity
