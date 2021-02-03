{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main where

import           Control.Monad                  ( forever )
import           Data.List                      ( intersperse )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )

import           Control.Exception              ( Exception )
import           Control.Exception.Base         ( throwIO )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import           Data.Typeable                  ( Typeable )
import qualified Database.SQLite.Simple        as SQLite
import           Database.SQLite.Simple         ( Connection
                                                , FromRow
                                                , Only(Only)
                                                , Query
                                                , ToRow
                                                , execute
                                                , execute_
                                                , field
                                                , fromRow
                                                , query
                                                , query_
                                                , toRow
                                                )
import           Database.SQLite.Simple.Types   ( Null(..) ) -- `Null` is a typeclass


import           Network.Socket                 ( AddrInfo(addrFlags)
                                                , AddrInfoFlag(AI_PASSIVE)
                                                , Socket
                                                , SocketType(Stream)
                                                , accept
                                                , addrFamily
                                                , close
                                                , defaultHints
                                                , defaultProtocol
                                                , getAddrInfo
                                                , listen
                                                , socket
                                                , withSocketsDo
                                                )
import           Network.Socket.ByteString      ( recv
                                                , sendAll
                                                )
import           Text.RawString.QQ              ( r )

main :: IO ()
main = withSocketsDo $ do
  addrInfos <- getAddrInfo (Just (defaultHints { addrFlags = [AI_PASSIVE] }))
                           Nothing
                           (Just "79")
  let serverAddr = head addrInfos
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  -- only one connection open at a time
  listen sock 1
  conn <- SQLite.open "finger.db"
  SQLite.close conn
  close sock

data User = User
  { userId        :: Integer
  , username      :: Text
  , shell         :: Text
  , homeDirectory :: Text
  , realName      :: Text
  , phone         :: Text
  }
  deriving (Eq, Show)

{- Boilerplate Typeclass Instances for 
   Marshalling & Unmarshalling Data to & from SQLite Database:
   `FrowRow User` & `ToRow User` -}
instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow User where
  toRow (User id' uName sh homeDir rName phone') =
    toRow $ (,,,,,) id' uName sh homeDir rName phone'

data DuplicateData = DuplicateData
  deriving (Eq, Show, Typeable)
instance Exception DuplicateData

-- type synonym for the tuples we insert to create a new user
type UserRow = (,,,,,) Null Text Text Text Text Text

createUsersQuery :: Query -- newtype wrapper for a `Text` value
createUsersQuery = [r|
CREATE TABLE IF NOT EXISTS users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
   username TEXT UNIQUE,
   shell TEXT,
   homeDirectory TEXT,
   realName TEXT,
   phone TEXT)
|]

insertUserQuery :: Query
insertUserQuery = "INSERT INTO users\
  \ VALUES (?, ?, ?, ?, ?, ?)"

allUsersQuery :: Query
allUsersQuery = "SELECT * from users"

getUserQuery :: Query
getUserQuery = "SELECT * from users where username = ?"

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn uName = do
  -- `Only` faciliates 1-value tuple 
  results <- query conn getUserQuery (Only uName)
  case results of
    []     -> return Nothing
    [user] -> return (Just user)
    _      -> throwIO DuplicateData

-- | Create database with 1 sample row containing my info
createDb :: IO ()
createDb = do
  let meRow =
        ( Null
        , "mnhthng"
        , "/bin/zsh"
        , "/home/mnhthng"
        , "MinhTu Thomas Hoang"
        , "111-222-999"
        ) :: UserRow
  conn <- SQLite.open "finger.db"
  execute_ conn createUsersQuery
  execute conn insertUserQuery meRow
  rows <- query_ conn allUsersQuery
  mapM_ print (rows :: [User])
  SQLite.close conn

showUserBS :: User -> ByteString
showUserBS (User _ uName sh homeDir rName _) = BS.concat
  [ "Login: ", encodeUtf8 uName, "\t\t\t\t"
  , "Name: ", encodeUtf8 rName, "\n"
  , "Directory: ", encodeUtf8 homeDir, "\t\t\t"
  , "Shell: ", encodeUtf8 sh, "\n"
  ]

-- | Use input database connection to query list of all users, 
-- | then transform them into a newline separated `Text` value.
-- | This `Text` will be encoded into UTF-8 `ByteString` and 
-- | be sent throuugh socket to the client.
returnUsers :: Connection -> Socket -> IO ()
returnUsers dbConn soc = do
  rows <- query_ dbConn allUsersQuery
  let uNames           = map username rows
      newLineSeperated = T.concat $ intersperse "\n" uNames
  sendAll soc (encodeUtf8 newLineSeperated)

-- | Use input database connection to query user whose username 
-- | matches `Text` input. 
returnUser :: Connection -> Socket -> Text -> IO ()
returnUser dbConn soc uName = do
  -- stripping is needed because the literal data 
  -- sent for a username query is "username\r\n"
  maybeUser <- getUser dbConn (T.strip uName)
  case maybeUser of
    -- no user by the given username is found
    Nothing -> do
      putStrLn ("Couldn't find matching user for username: " <> show uName)
      return ()
    Just u -> sendAll soc (showUserBS u)

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
  -- receive up to 1024 bytes
  msg <- recv soc 1024
  case msg of
    -- send the list of all user
    "\r\n" -> returnUsers dbConn soc
    -- send info of a single user
    name   -> returnUser dbConn soc $ decodeUtf8 name

handleQueries :: Connection -> Socket -> IO ()
handleQueries dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Connection established. Handling query..."
  handleQuery dbConn soc
  close soc