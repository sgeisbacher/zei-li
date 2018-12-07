module Context
( Ctx(Ctx)
, createCtx
, token
, httpLBSFunc
, endpointActivities
, endpointTimeTrackingStart
) where

import System.Directory (getHomeDirectory)
import System.IO.Error (catchIOError, ioError, userError)
import Data.String.Utils (strip, rstrip)
import Network.HTTP.Simple (Request, Response, httpLBS)
import qualified Data.ByteString.Lazy as B

data Ctx =
    Ctx {
        token :: String,
        endpointActivities :: String,
        endpointTimeTrackingStart :: String,
        httpLBSFunc :: Request -> IO (Response B.ByteString)  
    } 

loadToken :: [String] -> IO String
loadToken [] = ioError $ userError "could not read token from default-file-paths (./.zeitoken, ~/.zeitoken, /etc/zeitoken)"
loadToken (fileName:rest) = 
    catchIOError (strip . rstrip <$> readFile fileName) (\_ -> loadToken rest)

createCtx :: [Ctx -> Ctx] -> IO Ctx
createCtx options = do
    homeDir <- getHomeDirectory
    token <- loadToken [".zeitoken", homeDir ++ "/.zeitoken", "/etc/zeitoken"]
    let base = Ctx {
        token = token,
        endpointActivities = "https://api.timeular.com/api/v2/activities",
        endpointTimeTrackingStart = "https://api.timeular.com/api/v2/tracking/{activityId}/start",
        httpLBSFunc = httpLBS
    }
    return $ foldl (\ctx f -> f ctx) base options
