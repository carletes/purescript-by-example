module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head)
import Data.Maybe (Maybe)

type Entry =
  { firstName :: String
  , lastName :: String
  , address :: Address
  }

type Address =
  { street :: String
  , city :: String
  , state :: String
  }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry e = e.lastName <> ", " <>
              e.firstName <> ": " <>
              showAddress e.address

showAddress :: Address -> String
showAddress a = a.street <> ", " <>
                a.city <> ", " <>
                a.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry e b = Cons e b

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName b = head $ filter f b
  where
    f :: Entry -> Boolean
    f e = e.firstName == firstName && e.lastName == lastName
