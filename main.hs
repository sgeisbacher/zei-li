import System.Environment   
import System.Directory  
import System.IO  

import qualified Activities as Activities
import Context

commandDispatch :: [(String, Ctx -> [String] -> IO String)]  
commandDispatch = [ ("list", list)  
                  , ("help", printUsage)
                  ]

listDispatch :: [(String, Ctx -> [String] -> IO String)]  
listDispatch = [ ("activities", Activities.list)  
               ]

list :: Ctx -> [String] -> IO String
list ctx (entity:args) = case lookup entity listDispatch of
                        Nothing -> return $ "error: could not list unkown entity: " ++ entity
                        Just action -> action ctx args

main = do 
    ctx <- createCtx
    (command:args) <- getArgs  
    let (Just action) = lookup command commandDispatch  
    result <- action ctx args
    putStr result

printUsage :: Ctx -> [String] -> IO String
printUsage _ _ = return "usage: zei <command> <args>...\n\n\
    \examples:\n\
    \  list activities (<prefix>) - lists all your activities. filtered by prefix (optional)\n"