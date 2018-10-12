{-# LANGUAGE DeriveGeneric #-}

module Activities
( Activity
, list
, findByName
, Activities.id
, Activities.name
) where

import Data.Aeson
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Data.List
import Data.Char
import Data.String.Utils

import Context
import Uplink
import qualified Constants as C

newtype Activities =
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

strToLower :: String -> String
strToLower = map toLower 

startsWithCaseInsensitive :: String -> String -> Bool
startsWithCaseInsensitive prefix = startswith prefix . strToLower 

extractNames :: [Activity] -> [String]
extractNames = fmap name

filterByPrefix :: Maybe String -> [String] -> [String]
filterByPrefix Nothing activities = activities
filterByPrefix (Just prefix) activities = filter (startsWithCaseInsensitive prefix) activities

parsePrefix :: [String] -> Maybe String
parsePrefix [] = Nothing
parsePrefix (prefix:_) = Just prefix

is :: String -> Activity -> Bool
is [] _ = False
is otherName activity = Activities.name activity == otherName

findByName :: Ctx -> String -> IO (Maybe Activity)
findByName ctx name = do
    d <- eitherDecode <$> Uplink.get (token ctx) C.endpointActivities :: IO (Either String Activities)
    case d of 
        Left _ -> return Nothing
        Right result -> return $ Data.List.find (is name) $ activities result


list :: Ctx -> [String] -> IO String
list ctx args = do
    d <- eitherDecode <$> Uplink.get (token ctx) C.endpointActivities :: IO (Either String Activities)
    case d of 
        Left err -> return err 
        Right result -> return $ unlines . filterByPrefix (parsePrefix args) . extractNames $ activities result
