module Main where

import           Calculator
import           Control.Concurrent
import           Control.Exception     (finally)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C8
import           Network
import           System.IO             (BufferMode (LineBuffering), Handle,
                                        hClose, hIsEOF, hSetBuffering)

-- | Port number on which to run our server
port :: PortNumber
port = 4000

-- | Connects to the server. Sends data from stdin and prints response back to stdout.
client :: IO ()
client = do
  hndl <- connectTo "172.24.1.166" (PortNumber port)
  hSetBuffering hndl LineBuffering
  repeatUntil (action hndl) `finally` hClose hndl
 where
   action :: Handle -> IO Bool
   action h = do
     msg <- C8.getLine
     if B.null msg then return True else do
       C8.hPutStrLn h msg
       out <- C8.hGetLine h
       C8.putStrLn out
       return False

-- | Repeats an action until it return True.
repeatUntil :: Monad m => m Bool -> m ()
repeatUntil action = do
  b <- action
  if b then return () else repeatUntil action

main :: IO ()
main = client
