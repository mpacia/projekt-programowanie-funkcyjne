module Stack
  ( Stack -- Stack data structure.
  , empty   -- Creates empty Stack.
  , isEmpty -- checks whether Stack is empty.
  , push    -- push x s – put a value x on a top of a Stack s.
  , top     -- top s – returns a value from a top of a Stack s.
  , pop     -- pop s – returns a tuple consisting of a value from a top of a Stack s and the remaining part of a Stack s.
  , EmptyStackReadException -- | Thrown when reading from empty stack.
  ) where

import Control.Exception
-- interface
empty :: Stack a
isEmpty :: Stack a -> Bool
push :: a -> Stack a -> Stack a
top :: Stack a -> a
pop :: Stack a -> (a,Stack a)

-- implementation
newtype Stack a = MkStack [a] deriving Show -- hidden constructor (see the module export list)

data EmptyStackReadException = MkEmptyStackReadException String deriving Show
instance Exception EmptyStackReadException

empty = MkStack []
isEmpty (MkStack s) = null s
push x (MkStack s) = MkStack (x:s)

top (MkStack s) = head s
top (MkStack []) = throw $ MkEmptyStackReadException "Reading from empty stack."

pop (MkStack (s:ss)) = (s,MkStack ss)
pop (MkStack []) = throw $ MkEmptyStackReadException "Reading from empty stack."