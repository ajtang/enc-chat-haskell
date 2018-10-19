-- used as reference: https://github.com/joeyadams/haskell-chat-server-example/
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Foldable  as F
import qualified Data.Map       as Map

import Prelude hiding (id)

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (bracket_, finally)
import Control.Monad (forM_, forever, join)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Twilio

import Data.Int (Int64)
import Data.Map (Map)

import Network

import System.IO
import System.Environment (getEnv)

import Twilio
import Twilio.Messages


type Userid   = Int64
type UserName = String
type UserPass =  String
type UserCode =  String
type EnviroKey = String

data Message = Notice String
             | MessageFrom UserName String

-- data EnMessage = Notice String
--              | MessageFrom UserName String             

data Server = Server
    { serverUsers         :: TVar (Map Userid User)
    , serverUsersByName   :: TVar (Map UserName User)
    }

initializeServer :: IO Server
initializeServer =
    Server <$> newTVarIO Map.empty
           <*> newTVarIO Map.empty

data User = User
    { userid       :: Userid
    , userName     :: UserName
    , userPass     :: UserPass
--    , userCode     :: UserCode
    , userHandle   :: Handle
    , userSendChan :: TChan Message
    -- , userSendEnc  :: TChan EnMessage
    , userKicked   :: TVar (Maybe String)
    }

instance Eq User where
    a == b = userid a == userid b


-- fetchSid :: IO String
-- fetchSid = getEnv “TWILIO_ACCOUNT_SID”

-- fetchToken :: IO String
-- fetchToken = getEnv “TWILIO_AUTH_TOKEN”

--runTwilio’ :: IO String -> IO String -> Twilio a -> IO a

encrypt:: Int -> String -> String 
encrypt key msg 
    | key > 0   = encrypt (key - 1) (map shiftForward msg)
    | key == 0  = msg

-- Cipher Shifters --
    
shiftForward char
    | char == ' '   = '.'
    | char == '.'   = '.'
    | char == 'z'   = 'a'
    | char == 'Z'   = 'A'
    | otherwise     = succ char

shiftBack char
    | char == '.'   = ' '
    | char == ' '   = ' '
    | char == 'a'   = 'z'
    | char == 'A'   = 'Z'
    | otherwise      = pred char

-- Decrypt a message --

decrypt:: Int -> String -> String 
decrypt key msg
    | key > 0   = decrypt (key - 1) (map shiftBack msg)
    | key == 0  = msg

sendText:: T.Text -> IO()
sendText p = runTwilio' (getEnv "TWILIO_ACCOUNT_SID")
                  (getEnv "TWILIO_AUTH_TOKEN") $ do
  let body = PostMessage p "+16042451617" "Your Decryption Code is 123456"
  message <- post body
  liftIO $ print message

createUser :: Userid -> UserName -> UserPass -> Handle -> IO User
createUser id name pass handle =
    User <$> return id
           <*> return name
           <*> return pass
          -- <*> return code
           <*> return handle
           <*> newTChanIO
           <*> newTVarIO Nothing

broadcast :: Server -> Message -> STM ()
broadcast Server{..} msg =
    readTVar serverUsers >>= F.mapM_ (\user -> sendMessage user msg)

sendMessage :: User -> Message -> STM ()
sendMessage User{..} msg =
    writeTChan userSendChan msg

kickUser :: User -> String -> STM ()
kickUser User{..} reason =
    writeTVar userKicked $ Just reason

serve :: Server -> Userid -> Handle -> IO ()
serve server@Server{..} id handle = do
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle LineBuffering
    hPutStrLn handle "Input Username"
    name <- hGetLine handle
    hPutStrLn handle "What is your phone number? (Enter with '+Area Code')"
    pass <- hGetLine handle
   -- code <- hGetLine handle
    sendText (T.pack pass)
    if null name
        then hPutStrLn handle "Good Bye"
        else do
            user <- createUser id name pass handle
            bracket_ (atomically $ insertUser server user)
                     (atomically $ deleteUser server user)
                     (serveLoop server user)
    -- let body = PostMessage pass "+16042451617" "Hello, World!"
    -- message <- post body
    -- liftIO $ print message

-- | Register the user with the server.  If another user with the same name
-- is connected already, kick it.
insertUser :: Server -> User -> STM ()
insertUser server@Server{..}
             user@User{..} = do
    modifyTVar' serverUsers $ Map.insert userid user
    m <- readTVar serverUsersByName
    writeTVar serverUsersByName $! Map.insert userName user m
    case Map.lookup userName m of
        Nothing ->
            broadcast server $ Notice $
                userName ++ " has connected"
        Just existing -> do
            broadcast server $ Notice $
                userName ++ " has connected (kicking previous user)"
            kickUser existing $
                "Another user by the name of " ++ userName ++ " has connected"

-- | Delete the user.
deleteUser :: Server -> User -> STM ()
deleteUser server@Server{..}
             user@User{..} = do
    modifyTVar' serverUsers $ Map.delete userid
    m <- readTVar serverUsersByName
    case Map.lookup userName m of
        Just c | c == user -> do
            broadcast server $ Notice $ userName ++ " has disconnected"
            writeTVar serverUsersByName $! Map.delete userName m
        _ ->
            return ()

-- | Handle user I/O.
serveLoop :: Server -> User -> IO ()
serveLoop server@Server{..}
          user@User{..} = do
    done <- newEmptyMVar
    let spawnWorker io = forkIO (io `finally` tryPutMVar done ())

    recv_tid <- spawnWorker $ forever $ do
        msg <- hGetLine userHandle
        atomically $ broadcast server $ MessageFrom userName (encrypt 123456 msg)

    send_tid <- spawnWorker $
        let loop = join $ atomically $ do
                k <- readTVar userKicked
                case k of
                    Just reason -> return $
                        hPutStrLn userHandle $ "You have been kicked: " ++ reason
                    Nothing -> do
                        msg <- readTChan userSendChan
                        return $ do
                            handleMsg user msg
                            loop
         in loop

    takeMVar done
    mapM_ killThread [recv_tid, send_tid]

reserve :: Server -> Userid -> Handle -> IO ()
reserve server@Server{..} id handle = do

    hSetNewlineMode handle universalNewlineMode
        -- Swallow carriage returns sent by telnet users
    hSetBuffering handle LineBuffering

handleMsg :: User -> Message -> IO ()
handleMsg User{..} message =
    hPutStrLn userHandle $
        case message of
            Notice msg           -> "* " ++ msg
            MessageFrom name msg -> "<" ++ name ++ ">: " ++ msg


main :: IO ()
main = do
    server <- initializeServer
    sock <- listenOn $ PortNumber 8080
    putStrLn "Listening on port 8080"
    forM_ [1..] $ \id -> do
        (handle, host, port) <- accept sock
        putStrLn $ "Accepted connection from " ++ host ++ ":" ++ show port
        forkIO $ serve server id handle `finally` hClose handle
