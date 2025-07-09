{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import Network.Wai (Application)

import qualified Vendor

import Control.Concurrent (MVar)
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.Streaming.Network as Network
import qualified Network.Socket as Socket
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.Warp.Internal as WarpInternal

-- | Run an 'Application' on the given port.
run :: Warp.Port -> Application -> IO ()
run p = runSettings $ Warp.setPort p Warp.defaultSettings

{- | Run an 'Application' with the given 'Settings'.
This opens a listen socket on the port defined in 'Settings' and
calls 'runSettingsSocket'.
-}
runSettings :: Warp.Settings -> Application -> IO ()
runSettings settings app =
    Socket.withSocketsDo $
        Exception.bracket
            (Network.bindPortTCP (Warp.getPort settings) (Warp.getHost settings))
            Socket.close
            ( \socket -> do
                WarpInternal.setSocketCloseOnExec socket
                runSettingsSocket settings socket app
            )

{- | This installs a shutdown handler for the given socket and
calls 'runSettingsConnection' with the default connection setup action
which handles plain (non-cipher) HTTP.
When the listen socket in the second argument is closed, all live
connections are gracefully shut down.

The supplied socket can be a Unix named socket, which
can be used when reverse HTTP proxying into your application.

Note that the 'settingsPort' will still be passed to 'Application's via the
'serverPort' record.
-}
runSettingsSocket :: Warp.Settings -> Socket.Socket -> Application -> IO ()
runSettingsSocket settings socket app = do
    WarpInternal.settingsInstallShutdownHandler settings closeListenSocket
    runSettingsConnection settings getConn app
  where
    getConn = do
        -- (s, sa) <- Socket.accept socket
        (s, sa) <- (WarpInternal.settingsAccept settings) socket
        WarpInternal.setSocketCloseOnExec s
        -- NoDelay causes an error for AF_UNIX.
        Socket.setSocketOption s Socket.NoDelay 1 `Exception.catch` Vendor.throughAsync (pure ())
        conn <- WarpInternal.socketConnection settings s
        pure (conn, sa)

    closeListenSocket = Socket.close socket

{- | The connection setup action would be expensive. A good example
is initialization of TLS.
So, this converts the connection setup action to the connection maker
which will be executed after forking a new worker thread.
Then this calls 'runSettingsConnectionMaker' with the connection maker.
This allows the expensive computations to be performed
in a separate worker thread instead of the main server loop.
-}
runSettingsConnection :: Warp.Settings -> IO (WarpInternal.Connection, Socket.SockAddr) -> Application -> IO ()
runSettingsConnection settings getConn app = WarpInternal.runSettingsConnectionMaker settings getConnMaker app
  where
    getConnMaker = do
        (conn, sa) <- getConn
        pure (harpoonConnection conn, sa)

data ConnectionClosedError
    = ClientClosedConnection
    | ResponseWritten
    | ClientKeepsWriting
    deriving (Show, Eq, Enum, Ord)
    deriving anyclass (Exception.Exception)

harpoonConnection :: WarpInternal.Connection -> IO WarpInternal.Connection
harpoonConnection conn = do
    targetThread <- Concurrent.myThreadId

    -- Counts number of bytes which were read
    -- TODO: there might be more lightway abstration for atomic numbers,
    -- ideally something that could compile to HW instructions?
    bytesRead :: MVar Int <- Concurrent.newMVar 0
    -- This stores data which request thread might not have time to consume before monitoring thread kicked in
    lock :: MVar () <- Concurrent.newMVar ()

    monitorThreadId <- Concurrent.forkFinally (harpoonThread targetThread conn bytesRead 0 lock) (finalized targetThread)

    pure
        conn
            { WarpInternal.connClose = connClose monitorThreadId
            , WarpInternal.connRecv = connRecv bytesRead lock
            }
  where
    -- Close monitor and do perform normal teardown
    connClose tid =
        Exception.throwTo tid ResponseWritten
            >> WarpInternal.connClose conn

    -- Receive ByteString value from connection
    connRecv bytesRead lock = do
        -- Before we read from connection lets aquire the lock to it
        Concurrent.modifyMVar lock $ \() -> do
            bs <- WarpInternal.connRecv conn
            Concurrent.modifyMVar_ bytesRead $ \n ->
                pure $ ByteString.length bs + n
            pure ((), bs)

    finalized tid _res =
        putStrLn $ show tid <> " monitoring finished"

harpoonThread :: Concurrent.ThreadId -> WarpInternal.Connection -> MVar Int -> Int -> MVar () -> IO ()
harpoonThread tid conn bytesRead readSoFar lock = do
    Concurrent.threadDelay oneSecondInMicros

    n <- Concurrent.readMVar bytesRead
    putStrLn $ "read " <> show n
    putStrLn $ "read so far " <> show readSoFar

    -- Nothing is reading the data from the connection
    -- lets take over
    if n <= readSoFar
        then do
            closed <- Concurrent.modifyMVar lock $ \() -> do
                bs <- WarpInternal.connRecv conn
                -- If byte string was empty we know the client closed connection
                pure ((), ByteString.null bs)

            Exception.throwTo tid $ if closed then ClientClosedConnection else ClientKeepsWriting
        else continue n
  where
    continue n =
        harpoonThread tid conn bytesRead n lock

    oneSecondInMicros = 1000000
