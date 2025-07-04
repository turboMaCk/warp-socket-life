{-# LANGUAGE CPP #-}

module Vendor where

import Control.Exception
import Network.Socket

-- Network.Wai.Handler.Warp.Imports

isAsyncException :: (Exception e) => e -> Bool
isAsyncException e =
    case fromException (toException e) of
        Just (SomeAsyncException _) -> True
        Nothing -> False

throughAsync :: IO a -> SomeException -> IO a
throughAsync action (SomeException e)
    | isAsyncException e = throwIO e
    | otherwise = action

-- Network.Wai.Handler.Warp.Settings

defaultAccept :: Socket -> IO (Socket, SockAddr)
defaultAccept =
#if WINDOWS
    windowsThreadBlockHack . accept
#else
    accept
#endif
