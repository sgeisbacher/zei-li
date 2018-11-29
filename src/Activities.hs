{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
module Activities
( Activity
, list
-- , findByName
, Activities.id
, Activities.name
) where

import Data.Aeson
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Control.Monad.Reader
import Data.List
import Data.Char
import Data.String.Utils

import Context
import Uplink

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

-- findByName :: MonadReader Ctx m -> String -> m (IO (Maybe Activity))
-- findByName ctx name = do
--     d <- eitherDecode <$> Uplink.get ctx (endpointActivities ctx) :: IO (Either String Activities)
--     case d of 
--         Left _ -> return Nothing
--         Right result -> return $ Data.List.find (is name) $ activities result


list :: (MonadReader Ctx m, MonadIO m)  => [String] -> m String
list args = do
    ctx <- ask
    d <- eitherDecode <$> Uplink.get ctx (endpointActivities ctx) :: IO (Either String Activities)
    case d of 
        Left err -> return $ toIO err 
        Right result -> return $ toIO . unlines . filterByPrefix (parsePrefix args) . extractNames $ activities result

toIO :: String -> IO String
toIO str = do
    return str