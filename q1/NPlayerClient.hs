import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Network
import           System.IO
import           System.Random

port :: PortNumber
port = 4000

client :: IO ()
client = do
  hndl <- connectTo "172.24.1.166" (PortNumber port)
  hSetBuffering hndl LineBuffering
  let listener = do
                 e <- hIsEOF hndl
                 unless e $ do
                   msg <- hGetLine hndl
                   case msg of
                     "#" ->  do
                         rnd <- randomIO :: IO Int
                         hPutStrLn hndl $ show rnd
                         opponent <- read <$> hGetLine hndl
                         putStrLn $ "you:" ++ show rnd ++ "\nopponent: " ++ show opponent
                         if (opponent > rnd)
                           then hPutStrLn hndl "n"
                           else hPutStrLn hndl "y"
                         listener
                     "Q" -> hFlush hndl >> hClose hndl >> print "handle closed"
                     _ -> putStrLn msg >> listener
  listener

main :: IO ()
main = client
