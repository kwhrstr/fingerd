{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad (forever)
import           Data.List (intersperse)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Typeable
import           Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import           Database.SQLite.Simple.Types
import           Network.Socket hiding (close, recv)
import qualified Network.Socket as NS (close, bind)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Network.Socket.ByteString (recv, sendAll)
import           Text.RawString.QQ
import           Control.Monad.Catch
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Trans.Except


data User = User
  { userId        :: Integer
  , username      :: Text
  , shell         :: Text
  , homeDirectory :: Text
  , realName      :: Text
  , phone         :: Text
  } deriving (Eq, Show)
  
instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir realName phone)
    = toRow (id_, username, shell, homeDir, realName, phone)
    
createUsers :: Query
createUsers = [r|
CREATE TABLE IF NOT EXISTS  users
 (id INTEGER PRIMARY KEY  AUTOINCREMENT ,
  username TEXT UNIQUE ,
  shell TEXT,
  homeDirectory TEXT,
  realName TEXT,
  phone TEXT)
|]

insertUser :: Query
insertUser =
  "INSERT  INTO  users\
  \ VALUES  (?, ?, ?, ?, ?, ?)"

allUsers :: Query
allUsers =
  "SELECT * from users"

getUserQuery :: Query
getUserQuery =
  "SELECT * from users where username = ?"
  

data QueryException = DuplicateData | NotFoundData deriving (Eq, Show, Typeable)

instance Exception QueryException

type UserRow = (Null, Text, Text, Text, Text, Text)

getUser ::(MonadThrow m, MonadIO m) => Connection -> Text -> m User
getUser conn username = do
  results <- liftIO $ query conn getUserQuery (Only username)
  case results of
    [] -> throwM NotFoundData
    [user] -> return user
    _ -> throwM DuplicateData
    
createDatabase :: IO ()
createDatabase =
  bracket
    ( open "finger.db")
    SQLite.close
    ( \conn -> do
      execute_ conn createUsers
      execute conn insertUser meRow
      rows<- query_ conn allUsers
      mapM_ print (rows :: [User]))
  where
    meRow :: UserRow
    meRow = ( Null
            , "callen"
            , "/bin/zsh"
            , "/home/callen"
            , "Chris Allen"
            , "555-123-4567")

returnUsers :: Connection -> Socket -> IO ()
returnUsers dbConn soc = do
  rows <- query_ dbConn allUsers
  let userNames = map username rows
      newlineSeparated = T.concat $ intersperse "\n" userNames
  sendAll soc (encodeUtf8 newlineSeparated)


formatUser :: User -> ByteString
formatUser user = BS.concat
  [ "Login: ", e . username $ user, "\t\t\t\t"
  , "Name ", e . realName $ user, "\n"
  , "Directory: ", e . homeDirectory $ user, "\t\t\t"
  , "Shell: ", e . shell $ user, "\n"
  ]
  where
    e = encodeUtf8

returnUser :: (MonadCatch m, MonadIO m) => Connection -> Socket -> Text -> m ()
returnUser dbConn soc userName = do
  queried <- try $ getUser dbConn (T.strip userName)
  case queried of
    Right user -> liftIO $ sendAll soc (formatUser user)
    Left NotFoundData ->
      liftIO . putStrLn $
        "Couldn't find matching user for username: " ++ show userName
    Left DuplicateData ->
      liftIO . putStrLn $
        "duplicateError for username: " ++ show userName

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
  msg <- recv soc 1024
  case msg of
    "\r\n" -> returnUsers dbConn soc
    name -> returnUser dbConn soc (decodeUtf8 name)

handleQueries :: Connection -> Socket -> IO ()
handleQueries dbConn sock = forever $ bracket
  (fst <$> accept sock)
  NS.close
  (\soc -> do
   putStrLn "Got connection, handling query"
   handleQuery dbConn soc)




main :: IO ()
main = withSocketsDo $ do
  addInfos <- getAddrInfo
                (Just defaultHints
                       {addrFlags = [AI_PASSIVE]})
                Nothing
                (Just "79")
  let severaddr = head addInfos
  bracket
    (socket (addrFamily severaddr) Stream defaultProtocol)
    NS.close
    (\sock -> do
      NS.bind sock (addrAddress severaddr)
      listen sock 1
      conn <- open "finger.db"
      handleQueries conn sock
      SQLite.close conn)

