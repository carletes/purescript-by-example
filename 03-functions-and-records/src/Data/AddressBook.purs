module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy, null, tail)
import Data.Maybe (Maybe(..))

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

printEntry :: String -> String -> AddressBook -> Maybe String
printEntry f l b = map showEntry (findEntry f l b)

-- Exercise: Write a function which looks up an `Entry` given a street
-- address, by reusing the existing code in `findEntry`. Test your
-- function in PSCi

findEntryByAddress :: String -> String -> String -> AddressBook -> Maybe Entry
findEntryByAddress street city state b = head $ filter f b
  where
    f :: Entry -> Boolean
    f e = a.street == street && a.city == city && a.state == state
      where
        a = e.address

-- Exercise: Write a function which tests whether a name appears in a
-- `AddressBook`, returning a Boolean value. Hint: Use PSCi to find
-- the type of the `Data.List.null` function, which test whether a
-- list is empty or not

inAddressBook :: String -> String -> AddressBook -> Boolean
inAddressBook firstName lastName = null <<< filter f
  where
    f :: Entry -> Boolean
    f e = e.firstName == firstName && e.lastName == lastName

--- With explicit recursion and no `null`:

inAddressBook' :: String -> String -> AddressBook -> Boolean
inAddressBook' firstName lastName b = f (Just b)
  where
    f :: Maybe AddressBook -> Boolean
    f Nothing = false
    f (Just b') =
      case head b' of
        Nothing -> false
        Just e -> if e.firstName == firstName && e.lastName == lastName
                  then true
                  else f (tail b')

-- Exercise: Write a function `removeDuplicates` which removes
-- duplicate address book entries with the same first and last
-- names. Hint: Use PSCi to find the type of the `Data.List.nubBy`
-- function, which removes duplicate elements from a list based on an
-- equality predicate.

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates b = nubBy f b
  where
    f :: Entry -> Entry -> Boolean
    f e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName
