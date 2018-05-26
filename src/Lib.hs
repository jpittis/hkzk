{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
module Lib
    ( start
    ) where

import Servant
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as ByteString (pack)
import Data.Monoid ((<>))
import Data.ByteString (ByteString)
import Data.Aeson
import GHC.Generics
import Control.Monad.Reader
import Database.Zookeeper (Zookeeper, get, create, set, delete, getChildren,
                           AclList(..), ZKError)
import Database.Zookeeper.Pool (connect)
import Data.Pool
import Network.Wai.Handler.Warp (run)


type HKZKAPI =
       "entries"                                                :> Get    '[JSON] [Entry]
  :<|> "entries"                       :> ReqBody '[JSON] Entry :> Post   '[JSON] NoContent
  :<|> "entries" :> Capture "key" Text :> ReqBody '[JSON] Entry :> Post   '[JSON] NoContent
  :<|> "entries" :> Capture "key" Text                          :> Delete '[JSON] NoContent
  :<|> "entries" :> Capture "key" Text                          :> Get    '[JSON] Entry

api :: Proxy HKZKAPI
api = Proxy

newtype State = State { pool :: Pool Zookeeper }

initState :: IO State
initState = do
  pool <- connect "localhost:2181" 100 Nothing Nothing 1 1 1
  withResource pool $ \client ->
    create client "/entries" Nothing OpenAclUnsafe []
  return State { pool = pool }

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

data Entry = Entry
  { name  :: Text
  , value :: Maybe Text
  } deriving (Generic)

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
    toEntry name = Entry { name = pack name, value = Nothing }

createEntry :: Entry -> AppM NoContent
createEntry entry = do
  let path = unpack $ "/entries/" <> name entry
  let body = fmap encodeUtf8 (value entry)
  result <- withZookeeper $ \client ->
    create client path body OpenAclUnsafe []
  case result of
    Left err -> throwZKError err
    Right _ -> return NoContent

updateEntry :: Text -> Entry -> AppM NoContent
updateEntry name entry = do
  let path = unpack $ "/entries/" <> name
  let body = fmap encodeUtf8 (value entry)
  result <- withZookeeper $ \client ->
    set client path body Nothing
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
    Right (value, _) -> return Entry { name = name, value = fmap decodeUtf8 value }

withZookeeper :: (Zookeeper -> IO a) -> AppM a
withZookeeper f = do
  pool <- asks pool
  liftIO $ withResource pool f

throwZKError :: ZKError -> AppM a
throwZKError err =
  throwError err500 { errBody = ByteString.pack . show $ err }
