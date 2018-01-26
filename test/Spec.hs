import Test.QuickCheck
import Test.HUnit
import Prelude
import Stack
import Queue
import Dequeue

main :: IO Counts
main = do
    quickCheck (testingSequence :: Int -> Bool)
    quickCheck (fillWithNumers :: [Int] -> Bool)
    quickCheck (dequeueLenght :: [Char] -> Bool)
    runTestTT unitTests


leftOfPair (x,_)= x
rightOfPair (_,x)= x

testingSequence s = x == s
  where queue2 = addQ s emptyQ
        x = leftOfPair . remQ $ queue2

fillWithNumers [] = True
fillWithNumers s = checkEquality (fill s emptyQ) s
    where fill (s:ss) q = fill ss (addQ s q)
          fill [] q = q
          checkEquality q (s:ss) = (not (isEmptyQ q)) && (leftOfPair (remQ q) == s) && checkEquality (rightOfPair (remQ q)) ss
          checkEquality q [] = isEmptyQ q

dequeueLenght [] = isEmptyDEQ emptyDEQ
dequeueLenght s = dequeueLengthCheck s emptyDEQ
    where dequeueLengthCheck s q = lengthDEQ (fillDEQ s q) == length s
          fillDEQ (s:ss) q = fillDEQ ss (pushFrontDEQ q s)
          fillDEQ [] q = q

unitTest1 = TestCase (assertEqual "check whether after putting number 5 to and looking at the top of a stack we should see the same number as on the beggining:" 5 getPrepreparedStack)
unitTest2 = TestCase (assertEqual "check whether dequeue created with fromListDEQ is the same as created with pushBackDEQ" (uT2d1 testingList) (uT2d2 testingList))

testingList :: [Int]
testingList = [3,1,4,1,5,9,2,6,5,3,5,9]
uT2d1 = fromListDEQ
uT2d2 l = makeDEQ l emptyDEQ
    where makeDEQ (s:ss) d = makeDEQ ss (pushBackDEQ d s)
          makeDEQ [] d = d

getPrepreparedStack = top $ push 5 empty

unitTests = TestList [TestLabel "push and top" unitTest1, TestLabel "fromList and pushBack" unitTest2]