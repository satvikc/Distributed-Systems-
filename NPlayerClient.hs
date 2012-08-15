import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Network
import           System.IO
import Data.Random
import Data.Random.Source.DevRandom


port :: PortNumber
port = 4000
client :: IO ()
client = do
  hndl <- connectTo "172.24.1.166" (PortNumber port)
  hSetBuffering hndl NoBuffering
  done <- newEmptyMVar
  let spawnWorker io = forkIO (io `finally` tryPutMVar done ())
  let listener = do
                 e <- hIsEOF hndl
                 unless e $ do
                   msg <- hGetLine hndl
                   if (msg == "#")
                    then do
                         rnd <- runRVar (uniform 1 100) DevRandom :: IO Int
                         putStrLn $ "You Chose "++ show rnd
                         hPutStrLn hndl $ show rnd
                         let loop = do
                                    reply <- hGetLine hndl
                                    case reads reply ::[(Int,String)] of
                                      [(x,"")] -> do
                                        (putStrLn $ "Your Opponent Chose "++ show x)
                                        if (rnd <= x)
                                          then do
                                            putStrLn "I lost. Knocking myself out."
                                            hFlush hndl `finally` hClose hndl 
                                            putMVar done ()
                                          else listener
                                      _ -> putStrLn reply >> loop
                         loop
                    else  putStrLn msg >> listener
  recv_tid <- spawnWorker listener
  takeMVar done
  killThread recv_tid 

main :: IO ()
main = client
