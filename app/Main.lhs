> {-# LANGUAGE OverloadedStrings #-}

This is our simple HTTP server using WAI and WARP

- WAI - Is modular "Web Application Interface". It is an middleware based interface for HTTP apps
  - Similar to RACK in RUBY (Ruby A)
- WARP - A fast, light-weight web server for WAI applications.

> module Main where

We will need few primitives.
- Concurrency for (naive) modeling of workload
- Http types for convient names for HTTP statuses and headers
- Wai to get types that will define interface for our server
- Warp web server so we can listen for HTTP requests and response to requests

> import qualified Control.Concurrent as Concurrent
> import qualified Network.HTTP.Types as Http
> import qualified Network.HTTP.Types.Header as Header
> import qualified Network.Wai as Wai
> import qualified Network.Wai.Handler.Warp as Warp

Our library doesn't do anything interesting yet, but we can test
we can link to it already

> import qualified Server (someFunc)

This simple application just accepts any HTTP requests,
takes some time and then responed with status 200.

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

This is simple main where we just take our app and plug it to Warp server

> main :: IO ()
> main = do
>     let port = 3000
>     putStrLn $ "Listening on port " <> show port
>     -- We can call our library
>     Server.someFunc
>     -- We just start standard warp server
>     Warp.run port app
