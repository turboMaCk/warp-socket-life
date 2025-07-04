> {-# LANGUAGE OverloadedStrings #-}

> module Main where

> import qualified Control.Concurrent as Concurrent
> import qualified Network.HTTP.Types as Http
> import qualified Network.HTTP.Types.Header as Header
> import qualified Network.Wai as Wai

> import qualified Server

> app :: Wai.Application
> app _req f = do
>     wait 10
>     f $
>         Wai.responseLBS
>             Http.status200
>             [(Header.hContentType, "text/plain")]
>             "Hello world!\n"
>     where
>         wait :: Int -> IO ()
>         wait n = do
>             tid <- Concurrent.myThreadId
>             if n == 0
>                 then do
>                     putStrLn $ show tid <> " done!"
>                     pure ()
>                 else do
>                     putStrLn $ show tid <> " still running " <> show n
>                     Concurrent.threadDelay 1000000
>                     wait (n - 1)

Instead of WARP We're going to run server using our lib module:

> main :: IO ()
> main = do
>     let port = 3000
>     putStrLn $ "Listening on port " <> show port
>     Server.run port app
