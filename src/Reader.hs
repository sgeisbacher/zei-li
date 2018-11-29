{-# LANGUAGE FlexibleContexts #-}
module Reader where

import Control.Monad.Reader

data Context =
    Context {
        token :: String,
        endpoint :: String
    }

createCtx :: (String -> IO String) -> Context
createCtx f = Context {
    token = "token1234",
    endpoint = "http://endpoint.com/"
}

advanced :: String -> IO String
advanced fname = do
    content <- readFile $ "./" ++ fname
    return content
    

list :: MonadReader Context m => String -> m (IO String)
list _ = do
    ctx <- ask
    return $ advanced $ token ctx

commander :: MonadReader Context m => String -> m (IO String)
commander = list