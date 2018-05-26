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
    ) where

import           Prelude hiding (lookup)
import           Data.Text (Text)
import qualified Data.HashTable.IO as HT
import qualified Data.HashTable.IO as H
import           Data.Hashable (Hashable)
import           GHC.Generics

type Name = Text
type Body = Text

data Entry = Entry
  { name :: Name
  , body :: Maybe Body
  } deriving (Generic)

type HashTable = HT.BasicHashTable Name Entry

newtype Store = Store { hash :: HashTable }

fromList :: [Entry] -> IO Store
fromList entries = do
  hash <- H.fromList $ map toKeyValue entries
  return Store { hash = hash }
  where
    toKeyValue entry = (name entry, entry)

insert :: Store -> Entry -> IO ()
insert store entry = H.insert (hash store) (name entry) entry

delete :: Store -> Name -> IO ()
delete store = H.delete (hash store)

lookup :: Store -> Name -> IO (Maybe Entry)
lookup store = H.lookup (hash store)
