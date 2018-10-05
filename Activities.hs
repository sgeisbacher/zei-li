{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Activities
( Activity
, list
) where

import Data.Aeson
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Simple
import Network.HTTP.Types.Header
import qualified Data.ByteString.Char8 as C8
import qualified Data.CaseInsensitive as CI
import GHC.Generics
import Data.List
import Data.Char
import Data.String.Utils

import Context
import qualified Constants as C

data Activities =
    Activities { activities :: [Activity] } deriving (Show,Generic)

instance FromJSON Activities
instance ToJSON Activities

-- | Type of activity JSON entry in record syntax.
data Activity =
    Activity { id     :: !String
        , name        :: !String
        , color       :: !String
        , integration :: !String
        } deriving (Show,Generic)

instance FromJSON Activity
instance ToJSON Activity

jsonFile :: FilePath
jsonFile = "activities.json"
  
getJSONFromFile :: IO B.ByteString
getJSONFromFile = B.readFile jsonFile

getJSONFromServer :: Ctx -> IO B.ByteString
getJSONFromServer ctx = do
    req <- parseRequest C.endpointActivities
    let reqWithAuth = addRequestHeader hAuthorization (C8.pack $ "Bearer " ++ token ctx) req
    resp <- httpLBS reqWithAuth
    return $ getResponseBody resp

strToLower :: String -> String
strToLower = map toLower 

startsWithCaseInsensitive :: String -> String -> Bool
startsWithCaseInsensitive prefix str = startswith prefix . strToLower $ str

extractNames :: [Activity] -> [String]
extractNames = fmap name

filterByPrefix :: Maybe String -> [String] -> [String]
filterByPrefix (Nothing) activities = activities
filterByPrefix (Just prefix) activities = filter (startsWithCaseInsensitive prefix) activities

parsePrefix :: [String] -> Maybe String
parsePrefix [] = Nothing
parsePrefix (prefix:_) = Just prefix

list :: Ctx -> [String] -> IO String
list ctx args = do
    -- d <- (eitherDecode <$> getJSONFromFile) :: IO (Either String Activities)
    d <- (eitherDecode <$> (getJSONFromServer ctx)) :: IO (Either String Activities)
    case d of 
        Left err -> return err 
        Right result -> return $ unlines . (filterByPrefix $ parsePrefix args) . extractNames $ activities result
