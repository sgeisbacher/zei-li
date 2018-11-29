{-# LANGUAGE FlexibleContexts #-}
import System.Environment   
import System.Directory  
import System.IO  

-- import qualified Activities as A
-- import qualified TimeTracking as T
import Control.Monad.Reader
import Context

commandDispatch :: (MonadReader Ctx m, MonadIO m) => [(String, [String] -> m String)]
commandDispatch = [
    --  ("list", list)  
                --   , ("start", T.start)
                   ("help", printUsage)
                  ]

listDispatch :: (MonadReader Ctx m, MonadIO m) => [(String, [String] -> m String)]
listDispatch = [ 
    -- ("activities", A.list)  
               ]

list :: (MonadReader Ctx m, MonadIO m) => [String] -> m String
list (entity:args) = case lookup entity listDispatch of
                        Nothing -> return $ "error: could not list unkown entity: " ++ entity
                        Just action -> action args

main = do 
    ctx <- createCtx []
    args <- getArgs  
    putStr "asdf"
    -- result <- runReader (run args) ctx
    -- putStr result

run :: (MonadReader Ctx m, MonadIO m) => [String] -> m String
run (command:args) = do
    let (Just action) = lookup command commandDispatch  
    action args
    

printUsage :: (MonadReader Ctx m, MonadIO m) => [String] -> m String
printUsage _ = liftIO $ toIO "usage: zei <command> <args>...\n\n"
    -- \examples:\n\
    -- \  list activities (<prefix>) - lists all your activities. filtered by prefix (optional)\n\
    -- \  start <activity> - start time-tracking for given activity\n\n"

toIO :: String -> IO String
toIO str = do
    return str