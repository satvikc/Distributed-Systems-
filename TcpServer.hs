module TcpServer where

import           Calculator
import           Control.Concurrent
import           Control.Exception     (finally)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C8
import           Network
import           System.IO             (BufferMode (LineBuffering), Handle,
                                        hClose, hIsEOF, hSetBuffering)
import           Test.HUnit
import           Test.QuickCheck
 
-- | Port number on which to run our server
port :: PortNumber
port = 4000

-- | Runs the server. Takes a transformer which transforms the data on the socket.
runServer :: (B.ByteString -> IO B.ByteString) -> IO ()
runServer trans = listenOn (PortNumber port) >>= \sock -> (server sock trans `finally` sClose sock)

-- | Starts a server on the given socket with the given transformer.
server :: Socket -> (B.ByteString -> IO B.ByteString)-> IO ()
server socket trans = do
  putStrLn "Waiting for Connection"
  (h,_,_) <- accept socket
  hSetBuffering h LineBuffering
  forkIO $ (startServing h trans `finally` hClose h)
  server socket trans
 where
   startServing :: Handle -> (B.ByteString -> IO B.ByteString) -> IO ()
   startServing h t = do
     b <- hIsEOF h
     if b then return () else do
       p <- B.hGetLine h
       out <- t p
       C8.hPutStrLn h out
       startServing h t

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


calcServer :: IO ()
calcServer = runServer (\a -> return . C8.pack . eval $ C8.unpack a)




-- Tests

tests :: Test
tests = TestList $ map TestCase
  [assertEqual "add tests here"  1 (1::Int)
  ]

prop_empty :: Int -> Bool
prop_empty c1 = (c1::Int) == c1

runTests :: IO ()
runTests = do
  runTestTT tests
  quickCheck prop_empty

-- | For now, main will run our tests.
main :: IO ()
main = client
