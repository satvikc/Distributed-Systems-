{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Applicative          ((<$>), (<*>))
import           Control.Concurrent
import           Control.Exception            (bracket_, finally)
import           Control.Monad
import qualified Data.Foldable                as F
import           Data.Map                     (Map)
import qualified Data.Map                     as M
import           Data.Random
import           Data.Random.Source.DevRandom
import           Network
import           System.IO
import Data.IORef
import System.Random
import System.Environment (getArgs)


-- | Port number on which to run our server
port :: PortNumber
port = 4000


-- | Client Id each Client is Assigned
type ClientId = Int

-- | Client Name

type ClientName = String

-- | Random Number which client sends
type Number = Int

-- | Message can be of two types.
--  * Message sent from the Server
--  * Message sent from one client to the other
data Message = ServerMessage String
               | InitiateGame
               | OpponentsNumber Number
  deriving Show

-- | Server has a list of conencted Clients.
-- * By Id
-- * By Name
data Server  = Server
               { serverClients :: IORef (Map ClientId Client)
               , serverDummy :: Client
               }


-- | Initializes the Server
serverInit :: IO Server
serverInit = Server <$> newIORef M.empty <*> dummy

-- | Client
data Client = Client
              { clientId :: ClientId                  -- ^ Client Id
              , clientHandle :: Handle                -- ^ Connected Handle
              , clientOpponent :: IORef (Maybe Client) -- ^ Opponent
              }

-- Dummy Client which prints everything to stdout.
dummy :: IO Client
dummy = clientInit 0 stdout

instance Show Client where
  show Client{..} = "<Id: " ++ show clientId ++ ">"


instance Eq Client where
  a == b = clientId a == clientId b

-- | Initializes Client
clientInit :: ClientId -> Handle -> IO Client
clientInit cid h = Client cid h <$> newIORef Nothing

-- | Send Message To all the connected Clients
broadcast :: Server -> Message -> IO ()
broadcast Server{..} msg =
  readIORef serverClients >>= F.mapM_ (`sendMessage` msg)

-- | Add Client to the list of Connected Clients
insertClient :: Server -> Client -> IO ()
insertClient Server{..} client@Client{..} = atomicModifyIORef serverClients (\s -> (M.insert clientId client s,()) )

-- | Delete Client from the list of Connected Clients
deleteClient :: Server -> Client -> IO ()
deleteClient Server{..} Client{..} = atomicModifyIORef serverClients (\s -> (M.delete clientId s,()))

-- | Kicks Client Out
kickClient :: Server -> Client -> String -> IO ()
kickClient server c@Client{..} msg = do
  sendMessage c $ ServerMessage msg
  deleteClient server c
  hClose clientHandle

lostClient :: Server -> Client -> IO ()
lostClient s c = kickClient s c "You Lost the Game "


-- | Handle the Chat Message. This function actually is used to send message from channel to actual handle
sendMessage :: Client -> Message -> IO ()
sendMessage Client{..} message = hPutStrLn clientHandle $
        case message of
            ServerMessage msg           -> "* " ++ msg
            InitiateGame -> "#"
            OpponentsNumber number  -> show number

sendOpponent :: Client -> Message -> IO ()
sendOpponent Client{..} msg = do
  opponent <- readIORef clientOpponent
  case opponent of
    Nothing -> return ()
    Just c -> sendMessage c msg


connectClients :: Client -> Client -> IO ()
connectClients client1 client2 = do
   writeIORef (clientOpponent client1) (Just client2)
   writeIORef (clientOpponent client2) (Just client1)
   sendMessage client1 $ ServerMessage $ "Your Opponent is ClientId " ++ show (clientId client2)
   sendMessage client2 $ ServerMessage $ "Your Opponent is ClientId " ++ show (clientId client1)

-- | Disconnects Clients from each other
disconnectClient :: Client -> Client -> IO ()
disconnectClient client1 client2 = do
   writeIORef (clientOpponent client1) Nothing
   writeIORef (clientOpponent client2) Nothing

 -- | Disconnects Client from its partner
disconnect :: Client -> IO ()
disconnect client@Client{..} = do
   chatting <- readIORef clientOpponent
   case chatting of
     Nothing -> sendMessage client $ ServerMessage "Not opponent."
     Just c -> disconnectClient client c

-- | Server to handle the given client
serve :: Server -> ClientId -> Handle -> IO ()
serve server@Server{..} cid handle = do
    hSetNewlineMode handle universalNewlineMode
        -- Swallow carriage returns sent by telnet clients
    hSetBuffering handle LineBuffering
    client <- clientInit cid handle
    sendMessage client $ ServerMessage $ "Your Id is " ++ show cid
    insertClient server client

-- | Starts the server
runServer :: Int -> IO ()
runServer n = do
    server <- serverInit
    sock <- listenOn $ PortNumber port
    putStrLn $ "Listening on port " ++ show port
    let serv cid =  do
        (handle, host , p) <- accept sock
        putStrLn $ "Accepted connection from " ++ host ++ ":" ++ show p
        forkIO (serve server cid handle)
        if cid < n
          then serv (cid+1)
          else do
            print "waitloop in server"
            waitLoop n server
            print "wait loop ended"
            let play = do
                     list <- M.elems <$> (readIORef $ serverClients server)
                     print "Game  Started"
                     case list of
                       [c] -> do
                         putStrLn $ show c ++ " Won the Game."
                         kickClient server c "You Won the Game"
                       [] -> putStrLn "No body won."
                       _ -> do
                         games <- createGames server list
                         print games
                         mapM_ (checkGuesses server) games
                         print "Guesses Checked"
                         mapM_ (clearClients server) list 
                         print "Losers Kicked"
                         play
               in play
    serv 1 >> return () `finally` sClose sock
 where
   printOpponent :: Client -> IO ()
   printOpponent c@Client{..} = do
     opponent <- readIORef clientOpponent
     putStrLn $ show c ++ " <--> " ++ show opponent

   waitLoop t server = do
     clients <- readIORef (serverClients server)
     if ((M.size clients) < t)
       then  (yield >> waitLoop t server)
       else  (print $ M.size clients)
   checkGuesses :: Server -> (Client,Client) -> IO ()
   checkGuesses server (c1,c2) = do
     c1guess <- read <$> hGetLine (clientHandle c1)
     putStrLn $ show c1 ++ " guesses " ++ show c1guess
     if c2 == (serverDummy server)
       then do dguess <- randomIO :: IO Int
               sendMessage c1 $ OpponentsNumber dguess
               putStrLn $ "Dummy guesses " ++ show dguess
       else do c2guess <- read <$> hGetLine (clientHandle c2)
               sendMessage c1 $ OpponentsNumber c2guess
               sendMessage c2 $ OpponentsNumber c1guess
               putStrLn $ show c2 ++ " guesses " ++ show c2guess

   clearClients :: Server -> Client -> IO ()
   clearClients server c = do
     c1reply <- hGetLine (clientHandle c)
     when (c1reply == "n") $ lostClient server c

clearGames :: Server -> IO ()
clearGames Server{..} =
  F.mapM_ disconnect =<< readIORef serverClients

createGames :: Server -> [Client] -> IO [(Client,Client)]
createGames server@Server{..} clients = do
  shuffled <- runRVar (shuffle clients) DevRandom
  let games = gamify shuffled serverDummy
  initiateGames games
  return games
 where
   gamify :: [Client] -> Client -> [(Client,Client)]
   gamify [] _ = []
   gamify [x] d = [(x,d)]
   gamify (x:y:xs) d = (x,y):gamify xs d
   initiateGames :: [(Client,Client)] -> IO ()
   initiateGames [] = return ()
   initiateGames ((c,o):xs) = do
     connectClients c o
     sendMessage c InitiateGame
     sendMessage o InitiateGame
     initiateGames xs




main :: IO ()

main = do
  x:_ <- getArgs
  print x
  runServer (read x)
