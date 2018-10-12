module Context
( Ctx(Ctx)
, createCtx
, token
) where

import Data.String.Utils

newtype Ctx =
    Ctx {
        token :: String
    } deriving (Show)

createCtx :: IO Ctx
createCtx = do
    token <- strip . rstrip <$> readFile ".zeitoken"
    return Ctx { token=token }
