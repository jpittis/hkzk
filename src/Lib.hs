{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( start
    ) where

import           Store (Store, Entry(..))
import qualified Store

import           Servant
import           Data.Text (Text, unpack, pack)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as ByteString (pack)
import           Data.Monoid ((<>))
import           Data.ByteString (ByteString)
import           Data.Aeson
import           GHC.Generics
import           Control.Monad.Reader
import           Database.Zookeeper (Zookeeper, get, create, set, delete, getChildren,
                                     AclList(..), ZKError)
import           Database.Zookeeper.Pool (connect)
import           Data.Pool
import           Network.Wai.Handler.Warp (run)

import           Control.Concurrent.ReadWriteLock (RWLock)
import qualified Control.Concurrent.ReadWriteLock as RWL
import qualified Data.HashTable.IO as HT

type HKZKAPI =
       "entries"                                                :> Get    '[JSON] [Entry]
  :<|> "entries"                       :> ReqBody '[JSON] Entry :> Post   '[JSON] NoContent
  :<|> "entries" :> Capture "key" Text :> ReqBody '[JSON] Entry :> Post   '[JSON] NoContent
  :<|> "entries" :> Capture "key" Text                          :> Delete '[JSON] NoContent
  :<|> "entries" :> Capture "key" Text                          :> Get    '[JSON] Entry

api :: Proxy HKZKAPI
api = Proxy

type HashTable k v = HT.BasicHashTable k v

data State = State
  { pool  :: Pool Zookeeper
  , store :: Store
  , lock  :: RWLock
  }

initState :: IO State
initState = do
  pool <- connect "localhost:2181" 100 Nothing Nothing 1 1 1
  withResource pool $ \client ->
    create client "/entries" Nothing OpenAclUnsafe []
  store <- Store.fromList []
  lock <- RWL.new
  return State
    { pool  = pool
    , store = store
    , lock  = lock
    }

type AppM = ReaderT State Handler

server :: ServerT HKZKAPI AppM
server = getEntries
    :<|> createEntry
    :<|> updateEntry
    :<|> deleteEntry
    :<|> getEntry

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

app :: State -> Application
app s = serve api $ hoistServer api (nt s) server

start :: Int -> IO ()
start port =  do
  state <- initState
  run port $ app state

instance ToJSON Entry
instance FromJSON Entry

getEntries :: AppM [Entry]
getEntries = do
  result <- withZookeeper $ \client ->
    getChildren client "/entries" Nothing
  case result of
    Left err -> throwZKError err
    Right children -> return $ map toEntry children
  where
    toEntry :: String -> Entry
    toEntry name = Entry { name = pack name, body = Nothing }

createEntry :: Entry -> AppM NoContent
createEntry entry = do
  let path = unpack $ "/entries/" <> name entry
  let value = fmap encodeUtf8 (body entry)
  result <- withZookeeper $ \client ->
    create client path value OpenAclUnsafe []
  case result of
    Left err -> throwZKError err
    Right _ -> return NoContent

updateEntry :: Text -> Entry -> AppM NoContent
updateEntry name entry = do
  let path = unpack $ "/entries/" <> name
  let value = fmap encodeUtf8 (body entry)
  result <- withZookeeper $ \client ->
    set client path value Nothing
  case result of
    Left err -> throwZKError err
    Right _ -> return NoContent

deleteEntry :: Text -> AppM NoContent
deleteEntry name = do
  let path = unpack $ "/entries/" <> name
  result <- withZookeeper $ \client ->
    delete client path Nothing
  case result of
    Left err -> throwZKError err
    Right _ -> return NoContent

getEntry :: Text -> AppM Entry
getEntry name = do
  let path = unpack $ "/entries/" <> name
  result <- withZookeeper $ \client ->
    get client path Nothing
  case result of
    Left err -> throwZKError err
    Right (body, _) -> return Entry { name = name, body = fmap decodeUtf8 body }

withZookeeper :: (Zookeeper -> IO a) -> AppM a
withZookeeper f = do
  pool <- asks pool
  liftIO $ withResource pool f

throwZKError :: ZKError -> AppM a
throwZKError err =
  throwError err500 { errBody = ByteString.pack . show $ err }
