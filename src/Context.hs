module Context
( Ctx(Ctx)
, createCtx
, token
, httpLBSFunc
, endpointActivities
, endpointTimeTrackingStart
) where

import Data.String.Utils
import Network.HTTP.Simple
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as B

data Ctx =
    Ctx {
        token :: String,
        endpointActivities :: String,
        endpointTimeTrackingStart :: String,
        httpLBSFunc :: Request -> IO (Response B.ByteString)  
    } 

createCtx :: [Ctx -> Ctx] -> IO Ctx
createCtx options = do
    token <- strip . rstrip <$> readFile ".zeitoken"
    let base = Ctx {
        token = token,
        endpointActivities = "https://api.timeular.com/api/v2/activities",
        endpointTimeTrackingStart = "https://api.timeular.com/api/v2/tracking/{activityId}/start",
        httpLBSFunc = httpLBS
    }
    return $ foldl (\ctx f -> f ctx) base options
