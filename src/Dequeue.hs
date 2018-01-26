module Dequeue
 ( Dequeue -- Dequeue data structure.
 , emptyDEQ     -- Creates empty Dequeue.
 , isEmptyDEQ   -- Checks whether Dequeue is empty.
 , lengthDEQ    -- Returns length of a Dequeue.
 , firstDEQ     -- Returns first element of a Dequeue (Maybe a, where a is a type of data stored in Dequeue) or Nothing, if it doesn't exist.
 , lastDEQ      -- Returns last element of a Dequeue (Maybe a, where a is a type of data stored in Dequeue) or Nothing, if it doesn't exist.
 , takeFrontDEQ -- takeFrontDEQ n, applied to a Dequeue d, returns the prefix of d of length n, or list consisiting of all values of d  in original order if n > lengthDEQ d.
 , takeBackDEQ  -- takeBackDEQ n, applied to a Dequeue d, returns the last n values of d as a list, or all of d's values reversed as a list if n > lengthDEQ d.
 , pushFrontDEQ -- pushFrontDEQ q a – appends provided value a in the beginning of a returned Dequeue.
 , popFrontDEQ  -- popFrontDEQ q – returns tuple consisting of a first element of a Dequeue q and the Dequeue without this element
 , pushBackDEQ  -- pushBackDEQ q a – appends provided value a in the end of a returned Dequeue.
 , popBackDEQ   -- popBackDEQ q  – returns tuple consisting of a last element of a Dequeue q and the Dequeue without this element
 , fromListDEQ  -- fromListDEQ l – Creates Dequeue from list l.
 ) where

import Data.List
--interface
emptyDEQ :: Dequeue a
isEmptyDEQ :: Dequeue a -> Bool
lengthDEQ :: Dequeue a -> Int
firstDEQ :: Dequeue a -> Maybe a
lastDEQ :: Dequeue a -> Maybe a
takeFrontDEQ :: Int -> Dequeue a -> [a]
takeBackDEQ :: Int -> Dequeue a -> [a]
pushFrontDEQ :: Dequeue a -> a -> Dequeue a
popFrontDEQ :: Dequeue a -> Maybe (a, Dequeue a)
pushBackDEQ :: Dequeue a -> a -> Dequeue a
popBackDEQ :: Dequeue a -> Maybe (a, Dequeue a)
fromListDEQ :: [a] -> Dequeue a

--implementation
newtype Dequeue a = MkDequeue [a] deriving (Show, Eq)

emptyDEQ = MkDequeue []
isEmptyDEQ (MkDequeue q) = null q
lengthDEQ (MkDequeue q) = length q

firstDEQ (MkDequeue []) = Nothing
firstDEQ (MkDequeue (q:qq)) = Just q

lastDEQ (MkDequeue []) = Nothing
lastDEQ (MkDequeue q) = Just $ last q

takeFrontDEQ n (MkDequeue q) = take n q
takeBackDEQ n (MkDequeue q) = take n (reverse q)
pushFrontDEQ (MkDequeue q) x = MkDequeue (x:q)

popFrontDEQ (MkDequeue []) = Nothing
popFrontDEQ (MkDequeue (q:qq)) = Just $ (q, MkDequeue qq)

pushBackDEQ (MkDequeue q) x = MkDequeue (q ++ [x])

popBackDEQ (MkDequeue []) = Nothing
popBackDEQ (MkDequeue q) = Just $ (last q, MkDequeue (init q))

fromListDEQ l = MkDequeue l



