{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Applicative           ((<$>), (<*>))
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception             (bracket_, finally)
import           Control.Monad                 (filterM, forever, join)
import qualified Data.Foldable                 as F
import           Data.Map                      (Map)
import qualified Data.Map                      as M
import           Data.Maybe                    (isNothing, maybe)
import           Network
import           System.IO
import           Text.ParserCombinators.Parsec
import           Text.PrettyPrint              hiding (char)

-- | Port number on which to run our server
port :: PortNumber
port = 4000


-- | Client Id each Client is Assigned
type ClientId = Int

-- | Client Name
type ClientName = String

-- | Message can be of two types.
--  * Message sent from the Server
--  * Message sent from one client to the other
data Message = ServerMessage String
             | ClientMessage ClientName String
  deriving Show

data ServerCommand = Available
                   | ClientsNumber
                   | Request ClientId
                   | Accept ClientId
                   | Disconnect
                   | ConnectedTo
  deriving Show

data Server  = Server
               { serverClients :: TVar (Map ClientId Client)
               , serverClientsByName :: TVar (Map ClientName Client)
               }

serverInit :: IO Server
serverInit = Server <$> newTVarIO M.empty <*> newTVarIO M.empty

data Client = Client
              { clientId :: ClientId
              , clientName :: ClientName
              , clientHandle :: Handle
              , clientSendChan :: TChan Message
              , clientChatting :: TVar (Maybe Client)
              , clientRequested :: TVar (Maybe Client)
              , clientsWaiting :: TVar [Client]
              }

instance Show Client where
  show Client{..} = "Id: " ++ show clientId ++ " Name: " ++ clientName

instance Eq Client where
  a == b = clientId a == clientId b

clientInit :: ClientId -> ClientName -> Handle -> IO Client
clientInit cid name h =
  Client cid name h <$> newTChanIO <*> newTVarIO Nothing <*> newTVarIO Nothing <*> newTVarIO []

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg = writeTChan clientSendChan msg

sendChat :: Client -> Message -> STM ()
sendChat Client{..} msg = do
  other <- readTVar clientChatting
  case other of
    Nothing -> return ()
    Just c -> sendMessage c msg


broadcast :: Server -> Message -> STM ()
broadcast Server{..} msg =
    readTVar serverClients >>= F.mapM_ (\client -> sendMessage client msg)

sendWaiting :: Client -> Message -> STM ()
sendWaiting Client{..} msg = do
  chatting <- readTVar clientChatting
  waiting <- readTVar clientsWaiting
  mapM_ (\c -> if Just c /= chatting then sendMessage c msg else return ()) waiting

-- | Disconnects the given id client from chat.
disconnectClient :: Client -> Client -> STM ()
disconnectClient client1 client2 = do
  writeTVar (clientChatting client1) Nothing
  writeTVar (clientChatting client2) Nothing
  sendMessage client1 $ ServerMessage $ "ClientId " ++ (show $ clientId client2) ++ " disconnected"
  sendMessage client2 $ ServerMessage $ "ClientId " ++ (show $ clientId client1) ++ " disconnected"

disconnect :: Client -> STM ()
disconnect client@Client{..} = do
  chatting <- readTVar clientChatting
  case chatting of
    Nothing -> sendMessage client $ ServerMessage "Not connected to any client."
    Just c -> disconnectClient client c

available :: Map a Client -> STM ([(a,ClientId)])
available m = map (\(a,b) -> (a,clientId b)) <$> filterM emptyChat (M.toList m)
  where
    emptyChat (_,client) = do
      w <- readTVar $ clientChatting client
      return $ isNothing w

clientsNumber :: Map a Client -> STM Int
clientsNumber m = length <$> available m

requestClient :: Client -> Client -> STM ()
requestClient client1 client2 = do
  chatting <- readTVar $ clientChatting client2
  case chatting of
    Nothing -> do
      writeTVar (clientRequested client1) (Just client2)
      modifyTVar' (clientsWaiting client2) (\a -> client1:a)
      sendMessage client2 $ ServerMessage $ (show client1) ++ " wants to chat."
      sendMessage client1 $ ServerMessage $ "Your request has been sent"
    Just _ -> sendMessage client1 $ ServerMessage $ (show client2) ++ " is busy."

connectClients :: Client -> Client -> STM ()
connectClients client1 client2 = do
  writeTVar (clientChatting client1) (Just client2)
  writeTVar (clientChatting client2) (Just client1)
  writeTVar (clientRequested client1) Nothing
  writeTVar (clientRequested client2) Nothing
  sendMessage client1 $ ServerMessage $ "Connected to ClientId " ++ (show $ clientId client2)
  sendMessage client2 $ ServerMessage $ "Connected to ClientId " ++ (show $ clientId client1)

-- | client1 accepts the request of client2.
acceptClient :: Client -> Client -> STM ()
acceptClient client1 client2 = do
  req <- readTVar (clientRequested client2)
  if req == Just client1
    then do connectClients client1 client2
            sendWaiting client1 $ ServerMessage $  (show client1) ++ " not available for chat."
    else sendMessage client1 $ ServerMessage "Other client is either not available or has not requested to chat with you"


serveCommand :: Server -> Client -> ServerCommand -> STM ()
serveCommand Server{..} client@Client{..} sc = case sc of
  Available -> sendMessage client =<<
               ServerMessage . prettify <$> (available =<< readTVar serverClientsByName)

  ClientsNumber -> sendMessage client =<<
                   ServerMessage . show <$> (clientsNumber =<< readTVar serverClients)
  Request n -> do
    chatting <- readTVar clientChatting
    case chatting of
      Nothing -> do
        clients <- readTVar serverClients
        case M.lookup n clients of
          Nothing -> sendMessage client $ ServerMessage "No such client exist."
          Just c -> requestClient client c
      Just _ -> sendMessage client $ ServerMessage "You are already chatting with a client. Disconnect first to chat to another."
  Accept n -> do
    clients <- readTVar serverClients
    case M.lookup n clients of
      Nothing -> sendMessage client $ ServerMessage "No such client exist."
      Just c -> acceptClient client c
  Disconnect -> disconnect client
  ConnectedTo -> do
    ct <- readTVar clientChatting
    sendMessage client $ ServerMessage (show ct)
 where
   prettify = render  . hsep . map (text . show)




insertClient :: Server -> Client -> STM ()
insertClient Server{..}
             client@Client{..} = do
    modifyTVar' serverClients (M.insert clientId client)
    modifyTVar' serverClientsByName (M.insert clientName client)



deleteClient :: Server -> Client -> STM ()
deleteClient Server{..}
             client@Client{..} = do
    modifyTVar' serverClients $ M.delete clientId
    m <- readTVar serverClientsByName
    case M.lookup clientName m of
        Just c | c == client -> do
            sendChat client $ ServerMessage $ clientName ++ " has disconnected"
            writeTVar serverClientsByName $! M.delete clientName m
        _ ->
            return ()

serveLoop :: Server -> Client -> IO ()
serveLoop server@Server{..}
          client@Client{..} = do
    done <- newEmptyMVar
    let spawnWorker io = forkIO (io `finally` tryPutMVar done ())
    recv_tid <- spawnWorker $ forever $ do
        e <- hIsEOF clientHandle
        if e
          then return ()
          else do
               msg <- hGetLine clientHandle
               case parseServerCommand msg of
                 Nothing -> do
                   atomically $ sendChat client $ ClientMessage clientName msg
                   print msg
                 Just sc -> atomically $ serveCommand server client sc
    send_tid <- spawnWorker $
                let loop = join $ atomically $ do
                           msg <- readTChan clientSendChan
                           return $ do
                             handleMessage client msg
                             loop
                    in loop
    takeMVar done
    print "here"
    mapM_ killThread [recv_tid, send_tid]
    atomically $ do
      writeTVar clientChatting Nothing
      writeTVar clientRequested Nothing
    serveLoop server client


handleMessage :: Client -> Message -> IO ()
handleMessage Client{..} message = do
    hPutStrLn clientHandle $
        case message of
            ServerMessage msg           -> "* " ++ msg
            ClientMessage name msg -> "<" ++ name ++ ">: " ++ msg
    print "handle message success"

serve :: Server -> ClientId -> Handle -> IO ()
serve server@Server{..} cid handle = do
    hSetNewlineMode handle universalNewlineMode
        -- Swallow carriage returns sent by telnet clients
    hSetBuffering handle NoBuffering

    hPutStrLn handle "What is your name?"
    name <- hGetLine handle
    if null name
      then hPutStrLn handle "Bye, anonymous coward"
      else do
           rep <- atomically $ checkRepeat server name
           if rep
              then do
             hPutStrLn handle "Name Already Taken. Lets Try again"
             serve server cid handle
               else do
             hPutStrLn handle  $ "Welcome ! " ++ name
             client <- clientInit cid name handle
             bracket_ (atomically $ insertClient server client)
               (atomically $ deleteClient server client)
               (serveLoop server client)

checkRepeat :: Server -> ClientName -> STM Bool
checkRepeat Server{..} name = do
  names <- readTVar serverClientsByName
  case M.lookup name names of
    Nothing -> return False
    _ -> return True


runServer :: IO ()
runServer = do
    server <- serverInit
    sock <- listenOn $ PortNumber port
    putStrLn $ "Listening on port " ++ (show port)
    let serv cid =  do
        (handle, host , p) <- accept sock
        putStrLn $ "Accepted connection from " ++ host ++ ":" ++ show p
        forkIO $ serve server cid handle `finally` hClose handle
        serv (cid+1)
    serv 0 `finally` sClose sock


-- | Parsing Server Commands

pAvailable :: GenParser Char () ServerCommand
pAvailable = do
  string "available"
  return Available

pClientsNumber :: GenParser Char () ServerCommand
pClientsNumber = do
  string "number"
  return ClientsNumber

pDisconnect :: GenParser Char () ServerCommand
pDisconnect = do
  string "disconnect"
  return Disconnect

pRequest :: GenParser Char () ServerCommand
pRequest = do
  string "request"
  spaces
  number <- many1 $ oneOf ['0' .. '9']
  return $ Request (read number)

pAccept :: GenParser Char () ServerCommand
pAccept = do
  string "accept"
  spaces
  number <- many1 $ oneOf ['0' .. '9']
  return $ Accept (read number)


pConnectedTo :: GenParser Char () ServerCommand
pConnectedTo = do
  string "connectedTo"
  spaces
  return $ ConnectedTo

pServerCommand :: GenParser Char () ServerCommand
pServerCommand = do
  string "s"
  spaces
  char ':'
  spaces
  e <-      try pConnectedTo
       <|> try pClientsNumber
       <|> try pAvailable
       <|> try pDisconnect
       <|> try pRequest
       <|> pAccept
  optional eof
  return e

parseServerCommand :: String -> Maybe ServerCommand
parseServerCommand s = case parse pServerCommand "" s of
  Right sc -> Just sc
  _ -> Nothing


main :: IO ()
main = runServer
