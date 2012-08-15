{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Applicative          ((<$>), (<*>))
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception            (bracket_, finally)
import           Control.Monad
import qualified Data.Foldable                as F
import           Data.Map                     (Map)
import qualified Data.Map                     as M
import           Data.Random
import           Data.Random.Source.DevRandom
import           Network
import           System.IO


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
               { serverClients :: TVar (Map ClientId Client)
               , serverGames :: TVar [(Client,Client)]
               , serverDummy :: Client
               , serverInitiateGo :: MVar ()   -- ^ To syncronize on games
               , serverInitiateDone :: MVar () -- ^ To synchronize on games
               }

-- | Initializes the Server
serverInit :: IO Server
serverInit = Server <$> newTVarIO M.empty <*> newTVarIO [] <*> dummy <*> newMVar () <*> newMVar ()

-- | Client
data Client = Client
              { clientId :: ClientId                  -- ^ Client Id
              , clientHandle :: Handle                -- ^ Connected Handle
              , clientSendChan :: TChan Message       -- ^ Channel on which to send message
              , clientOpponent :: TVar (Maybe Client) -- ^ Chatting Client
              }

-- Dummy Client which prints everything to stdout.
dummy :: IO Client
dummy = clientInit 0 stdout

instance Show Client where
  show Client{..} = "<Id: " ++ show clientId ++ ">"

printOpponent :: Client -> IO ()
printOpponent Client{..} = print =<< (atomically $ readTVar clientOpponent)

instance Eq Client where
  a == b = clientId a == clientId b

-- | Initializes Client
clientInit :: ClientId -> Handle -> IO Client
clientInit cid h =
  Client cid h <$> newTChanIO <*> newTVarIO Nothing

-- | Send Message to the Client
sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} = writeTChan clientSendChan

-- | Send Chat Message to the Opponent of the Client
sendOpponent :: Client -> Message -> STM ()
sendOpponent Client{..} msg = maybe (return ()) (`sendMessage` msg) =<< readTVar clientOpponent


-- | Send Message To all the connected Clients
broadcast :: Server -> Message -> STM ()

broadcast Server{..} msg =
  readTVar serverClients >>= F.mapM_ (`sendMessage` msg)


-- | Disconnects Clients from each other
disconnectClient :: Client -> Client -> STM ()
disconnectClient client1 client2 = do
  writeTVar (clientOpponent client1) Nothing
  writeTVar (clientOpponent client2) Nothing


-- | Disconnects Client from its partner
disconnect :: Client -> STM ()
disconnect client@Client{..} = do
  chatting <- readTVar clientOpponent
  case chatting of
    Nothing -> sendMessage client $ ServerMessage "Not opponent."
    Just c -> disconnectClient client c


connectClients :: Client -> Client -> STM ()
connectClients client1 client2 = do
  writeTVar (clientOpponent client1) (Just client2)
  writeTVar (clientOpponent client2) (Just client1)
  sendMessage client1 $ ServerMessage $ "Your Opponent is ClientId " ++ show (clientId client2)
  sendMessage client2 $ ServerMessage $ "Your Opponent is ClientId " ++ show (clientId client1)

-- | Add Client to the list of Connected Clients
insertClient :: Server -> Client -> STM ()
insertClient Server{..} client@Client{..} = modifyTVar' serverClients (M.insert clientId client)

-- | Delete Client from the list of Connected Clients
deleteClient :: Server -> Client -> STM ()
deleteClient Server{..} Client{..} = modifyTVar' serverClients $ M.delete clientId

-- | Kicks Client Out

kickClient :: Client -> IO ()
kickClient Client{..} = hClose clientHandle

-- | Server Loop to serve the client requests
serveLoop :: Server -> Client -> IO ()
serveLoop Server{..}
          client@Client{..} = do
    done <- newEmptyMVar
    let spawnWorker io = forkIO (io `finally` tryPutMVar done ())

    recv_tid <- spawnWorker $ forever $ do

                  msg <- hGetLine clientHandle
                  print msg
                  case reads msg of
                      [(x,"")] -> atomically $ sendOpponent client $ OpponentsNumber x
                      _ -> do
                        atomically $ sendMessage client $ ServerMessage "Wrong Format of Number. You will be knocked out."
                        kickClient client



    send_tid <- spawnWorker $
                let loop = join $ atomically $ do
                           msg <- readTChan clientSendChan
                           return $ do
                             handleMessage client msg
                             loop
                    in loop

    cleaner <- spawnWorker $
               let loop = do
                     st <- hIsEOF clientHandle
                     print st
                     when st loop
                   in loop
    takeMVar done
    mapM_ killThread [recv_tid, send_tid, cleaner]

-- | Handle the Chat Message. This function actually is used to send message from channel to actual handle
handleMessage :: Client -> Message -> IO ()
handleMessage Client{..} message = hPutStrLn clientHandle $
        case message of
            ServerMessage msg           -> "* " ++ msg
            InitiateGame -> "#"
            OpponentsNumber number  -> show number

-- | Server to handle the given client
serve :: Server -> ClientId -> Handle -> IO ()
serve server@Server{..} cid handle = do
    hSetNewlineMode handle universalNewlineMode
        -- Swallow carriage returns sent by telnet clients
    hSetBuffering handle NoBuffering
    client <- clientInit cid handle
    atomically $ sendMessage client $ ServerMessage $ "Your Id is " ++ show cid
    bracket_ (atomically $ insertClient server client)
      (atomically $ do
        disconnect client
        deleteClient server client)
      (serveLoop server client)

-- | Starts the server
runServer :: Int -> IO ()
runServer n = do
    server <- serverInit
    sock <- listenOn $ PortNumber port
    putStrLn $ "Listening on port " ++ show port
    let serv cid =  do
        (handle, host , p) <- accept sock
        putStrLn $ "Accepted connection from " ++ host ++ ":" ++ show p
        forkIO (serve server cid handle `finally` hClose handle)
        if cid < n
          then serv (cid+1)
          else let play = do
                     print "Clearing Clients"
                     atomically $ clearGames server
                     print "Creating Games"
                     list <- createGames server
                     print "Game  Started"
                     mapM_ printOpponent . M.elems =<< (atomically $ readTVar (serverClients
                                                                     server))

                     case list of
                       [c] -> do
                         atomically $ sendMessage c $ ServerMessage "You Won then game"
                         putStrLn $ show c ++ " Won the Game."
                       [] -> putStrLn "No body won."
                       _ -> waitLoop (length list) server >> play
               in play
    serv 1 >> return () `finally` sClose sock
 where
   waitLoop t server = do
     clients <- atomically $ readTVar (serverClients server)
     if (M.size clients) < t
       then return ()
       else yield >> waitLoop t server

clearGames :: Server -> STM ()
clearGames Server{..} =
  F.mapM_ (\client -> writeTVar (clientOpponent client) Nothing) =<< readTVar serverClients

createGames :: Server -> IO [Client]
createGames server@Server{..} = do
  clients <-  M.elems <$> readTVarIO serverClients
  shuffled <- runRVar (shuffle clients) DevRandom
  let games = gamify shuffled serverDummy
  atomically $ (initiateGames games >> (broadcast server $ ServerMessage "Round Started"))
  dummyRandom <- runRVar (uniform 1 100) DevRandom
  atomically $ sendOpponent serverDummy (OpponentsNumber dummyRandom)
  return clients
 where
   gamify :: [Client] -> Client -> [(Client,Client)]
   gamify [] _ = []
   gamify [x] d = [(x,d)]
   gamify (x:y:xs) d = (x,y):gamify xs d
   initiateGames :: [(Client,Client)] -> STM ()
   initiateGames [] = return ()
   initiateGames ((c,o):xs) = do
     connectClients c o
     sendMessage c InitiateGame
     sendMessage o InitiateGame
     initiateGames xs




main :: IO ()

main = runServer 3
