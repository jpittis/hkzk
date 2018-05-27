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

toList :: Store -> IO [Entry]
toList store =
  map snd <$> H.toList (hash store)

insert :: Store -> Name -> Entry -> IO ()
insert store = H.insert (hash store)

delete :: Store -> Name -> IO ()
delete store = H.delete (hash store)

lookup :: Store -> Name -> IO (Maybe Entry)
lookup store = H.lookup (hash store)
