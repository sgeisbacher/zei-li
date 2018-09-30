module Context
( Ctx(Ctx)
, createCtx
, token
) where

import Data.String.Utils

data Ctx =
    Ctx {
        token :: String
    } deriving (Show)

createCtx :: IO Ctx
createCtx = do
    token <- fmap (strip . rstrip) $ readFile ".zeitoken"
    return Ctx { token=token }
