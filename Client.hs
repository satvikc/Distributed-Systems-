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
              if e
                then return ()
                else do
                     msg <- hGetLine hndl
                     putStrLn msg
  send_tid <- spawnWorker $ forever $ do
    w <- getLine
    hPutStrLn hndl w
  takeMVar done
  print "here"
  mapM_ killThread [recv_tid, send_tid]

main :: IO ()
main = client
