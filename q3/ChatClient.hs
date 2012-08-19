module Main where 

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Network
import           System.IO

port :: PortNumber
port = 4000
client :: IO ()
client = do
  hndl <- connectTo "172.24.1.166" (PortNumber port)
  hSetBuffering hndl NoBuffering
  done <- newEmptyMVar
  let spawnWorker io = forkIO (io `finally` tryPutMVar done ())
  recv_tid <- spawnWorker $ forever $ do
    e <- hIsEOF hndl
    unless e $ do msg <- hGetLine hndl
                  putStrLn msg
  send_tid <- spawnWorker $ forever $ do
    w <- getLine
    when (w=="quit") (tryPutMVar done () >> return ())
    hPutStrLn hndl w
    hFlush hndl
  takeMVar done
  mapM_ killThread [recv_tid, send_tid] `finally` hClose hndl

main :: IO ()
main = client
