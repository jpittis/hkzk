{-# LANGUAGE DeriveGeneric #-}
module Store
    ( Entry(..)
    , Name
    , Body
    , Store
    , fromList
    , insert
    , delete
    , lookup
    , toList
    , replace
    ) where

import           Prelude hiding (lookup)
import           Data.Text (Text)
import qualified Data.HashTable.IO as HT
import qualified Data.HashTable.IO as H
import           Data.Hashable (Hashable)
import           GHC.Generics
import           Data.IORef

type Name = Text
type Body = Text

data Entry = Entry
  { name :: Name
  , body :: Maybe Body
  } deriving (Generic)

type HashTable = HT.BasicHashTable Name Entry

newtype Store = Store { hash :: IORef HashTable }

fromList :: [Entry] -> IO Store
fromList entries = do
  hash <- hashFromList entries
  ref  <- newIORef hash
  return Store { hash = ref }

toList :: Store -> IO [Entry]
toList store = do
  hash <- readIORef (hash store)
  map snd <$> H.toList hash

insert :: Store -> Name -> Entry -> IO ()
insert store name entry = do
  hash <- readIORef (hash store)
  H.insert hash name entry

delete :: Store -> Name -> IO ()
delete store name = do
  hash <- readIORef (hash store)
  H.delete hash name

lookup :: Store -> Name -> IO (Maybe Entry)
lookup store name = do
  hash <- readIORef (hash store)
  H.lookup hash name

replace :: Store -> [Entry] -> IO ()
replace store entries = do
  new <- hashFromList entries
  writeIORef (hash store) new

hashFromList :: [Entry] -> IO HashTable
hashFromList entries =
  H.fromList $ map toKeyValue entries
  where
    toKeyValue entry = (name entry, entry)
