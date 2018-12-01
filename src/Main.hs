import System.Environment (getArgs)

import qualified Activities as A
import qualified TimeTracking as T
import Context

commandDispatch :: [(String, Ctx -> [String] -> IO String)]  
commandDispatch = [ ("list", list)  
                  , ("start", T.start)
                  , ("help", printUsage)
                  ]

listDispatch :: [(String, Ctx -> [String] -> IO String)]  
listDispatch = [ ("activities", A.list)  
               ]

list :: Ctx -> [String] -> IO String
list ctx [] = return "missing entity in list command"
list ctx (entity:args) = case lookup entity listDispatch of
                        Nothing -> return $ "error: could not list unkown entity: " ++ entity
                        Just action -> action ctx args

main = do 
    ctx <- createCtx []
    args <- getArgs  
    result <- run ctx args
    putStr result

run :: Ctx -> [String] -> IO String
run ctx [] = run ctx ["help"]
run ctx (command:args) = do
    let (Just action) = lookup command commandDispatch  
    action ctx args

printUsage :: Ctx -> [String] -> IO String
printUsage _ _ = return "usage: zei <command> <args>...\n\n\
    \examples:\n\
    \  list activities (<prefix>) - lists all your activities. filtered by prefix (optional)\n\
    \  start <activity> - start time-tracking for given activity\n\n"