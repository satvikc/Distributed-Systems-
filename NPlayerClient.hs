import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Network
import           System.IO
import           System.Random

port :: PortNumber
port = 4000

client :: io ()
client = do
  hndl <- connectto "172.24.1.166" (portnumber port)
  hsetbuffering hndl linebuffering
  let listener = do
                 e <- hiseof hndl
                 unless e $ do
                   msg <- hgetline hndl
                   case msg of
                     "#" ->  do
                         rnd <- randomio :: io int
                         hputstrln hndl $ show rnd
                         opponent <- read <$> hgetline hndl
                         putstrln $ "you:" ++ show rnd ++ "\nopponent: " ++ show opponent
                         if (opponent > rnd)
                           then hputstrln hndl "n"
                           else hputstrln hndl "y"
                         listener
                     "q" -> hflush hndl >> hclose hndl >> print "handle closed"
                     _ -> putstrln msg >> listener
  listener

main :: io ()
main = client
