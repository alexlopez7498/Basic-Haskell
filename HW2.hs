-- CptS 355 - Spring 2024 Assignment 2
-- Please include your name and the names of the students with whom you discussed any of the problems in this homework

module HW2
     where
import Data.Void (vacuous)


{- 1-  merge2 & merge2Tail & mergeN - 22% -}
--merge2
merge2 :: Ord a => [a] -> [a] -> [a]
merge2 [] [] = [] -- if both lists are empty we return an empty list
merge2 (x:xs) [] = (x:xs) -- if the second list is empty then we return the first list
merge2 [] (y:ys) = (y:ys) -- if the first list is empty then we return the second list
merge2 (x:xs) (y:ys) -- case if both lists aren't empty
     | x <= y = x : merge2 xs (y:ys) -- check if head of first list is less than or equal to head of second list and if it is then we append and recursively call the function with the tail of the first list
     | otherwise = y : merge2 (x:xs) ys -- otherwise we append the head of the second list and recursively go into the function with tail of second list

--merge2Tail
merge2Tail :: Ord a => [a] -> [a] -> [a]
merge2Tail xs ys = mergeHelper xs ys [] -- using a function for tail recursion
  where
    mergeHelper :: Ord a => [a] -> [a] -> [a] -> [a]
    mergeHelper [] [] acc = acc  -- we return the accumlator if both list are empty 
    mergeHelper (x:xs) [] acc = revAppend acc (x:xs) -- we call revAppend if one of the lists is empty
    mergeHelper [] (y:ys) acc = revAppend acc (y:ys) -- we call revAppend if one of the lists is empty
    mergeHelper (x:xs) (y:ys) acc
         | x <= y = mergeHelper xs (y:ys) (x:acc)  -- check if head of first list is less than or equal to head of second list we recursively call merageHelper and append x into accumlator
         | otherwise = mergeHelper (x:xs) ys (y:acc)  -- otherwise we recursively go into mergeHelper but this time we append y to accumlator
    revAppend :: [a] -> [a] -> [a]
    revAppend [] ys = ys
    revAppend (x:xs) ys = revAppend xs (x:ys)




--mergeN
mergeN :: Ord a => [[a]] -> [a]
mergeN = foldl merge2 [] -- we use foldl to start from the left and for every list we call it into merge2 function

{- 2 - getInRange & countInRange - 18% -}

getInRange :: Ord a => a -> a -> [a] -> [a]
getInRange value1 value2 list = filter (\element -> element > value1 && element < value2) list -- using filter we filter out everything that is less the first value and greater than the second value, so all we have is everything in between

--countInRange
countInRange :: Ord a => a -> a -> [[a]] -> Int
countInRange value1 value2 list = sum (map (\elementList -> length (getInRange value1 value2 elementList)) list) -- first we use getInRange to get every value we between the chosen value, then get the length of each list so then can just get the sum of the list which would give us the answer
{- 3 -  addLengths & addAllLengths - 18% -}

data LengthUnit =  INCH  Int | FOOT  Int | YARD  Int
                   deriving (Show, Read, Eq)
-- addLengths
addLengths :: LengthUnit -> LengthUnit -> LengthUnit
addLengths (INCH x) (INCH y) = INCH (x + y)  -- we check every combination and do the correct arthmetic for each
addLengths (FOOT x) (FOOT y) = INCH (x * 12 + y * 12)
addLengths (FOOT x) (INCH y) = INCH (x * 12 + y)
addLengths (INCH y) (FOOT x) = INCH (x * 12 + y)
addLengths (YARD x) (INCH y) = INCH (x * 36 + y)
addLengths (INCH x) (YARD y) = INCH (y * 36 + x)
addLengths (YARD x) (FOOT y) = INCH (x * 36 + y * 12)
addLengths (FOOT x) (YARD y) = INCH (y * 36 + x * 12)
addLengths (YARD x) (YARD y) = INCH (y * 36 + x * 36)
-- addAllLengths
addAllLengths :: [[LengthUnit]] -> LengthUnit
addAllLengths lengthLists = (foldl (addLengths) (INCH 0) . map (foldl (addLengths) (INCH 0))) lengthLists -- the function first uses maps on lengthLists and accumlates it and then uses that result in left side of the period to give the answer


{-4 - sumTree and createSumTree - 22%-}

data Tree a = LEAF a | NODE a  (Tree a)  (Tree a)
              deriving (Show, Read, Eq)

--sumTree
sumTree :: Num p => Tree p -> p
sumTree (LEAF x) = x -- if were at a leaft then we just return its value
sumTree (NODE x left right) = sumTree left + sumTree right -- when we are at a node then we recursively add the sumTree of the left and right
  
--createSumTree
createSumTree :: Num a => Tree a -> Tree a
createSumTree (LEAF x) = LEAF x -- if were at a leaft then we just return its value
createSumTree (NODE x left right) = NODE (sumTree leftSum + sumTree rightSum) leftSum rightSum -- we use sumTree to variable leftSum and rightSum which recursively calls createSumTree
  where
    leftSum = createSumTree left -- using these to clean up the code a bit
    rightSum = createSumTree right



{-5 - foldListTree - 20%-}
data ListTree a = ListLEAF [a] | ListNODE [ListTree a]
                  deriving (Show, Read, Eq)

foldListTree :: (a -> a -> a) -> a -> ListTree a -> a
foldListTree f base (ListLEAF x) = foldl f base x -- if were at a leaf when we foldl the list
foldListTree f base (ListNODE child) = foldl (foldHelper f) base child -- if where a node then we foldl but first we go the helper function
  where
    foldHelper :: (a -> a -> a) -> a -> ListTree a -> a
    foldHelper f acc subTree = foldListTree f acc subTree -- here we recursively can foldListTree and have an accumlator to keep track of the value

{- 6- Create two tree values :  Tree Integer  and  listTree a ;  Both trees should have at least 3 levels. -}
treeInt1 :: Tree Integer
treeInt1 = NODE 7
         (NODE 4 (NODE 2 (LEAF 3) (LEAF 1)) (LEAF 6))
         (NODE 3 (LEAF 8) (LEAF 12))
treeInt2 :: Tree Integer
treeInt2 = NODE 0
         (NODE 0 (NODE 0 (LEAF 3) (LEAF 1)) (LEAF 6))
         (NODE 0 (LEAF 2) (LEAF 12))

treeList1 :: ListTree Integer
treeList1 = ListNODE
 [ ListNODE [ ListLEAF [1,2,3],ListLEAF [4,5],ListNODE([ListLEAF [6], ListLEAF []]) ],
   ListNODE [],
   ListLEAF [7,8],
   ListNODE [ListLEAF [], ListLEAF []] ]  

treeList2 :: ListTree Integer
treeList2 = ListNODE
 [ ListNODE [ ListLEAF [2],ListLEAF [3,5],ListNODE([ListLEAF [6], ListLEAF [2]]) ],
   ListNODE [],
   ListLEAF [2,7],
   ListNODE [ListLEAF [1], ListLEAF [2,2]] ]  



main :: IO ()
main = do
    let t1 = NODE 1
                  (NODE 2 (NODE 3 (LEAF 4) (LEAF 5)) (LEAF 6))
                  (NODE 7 (LEAF 8) (LEAF 9))
        t3 = NODE 0
                  (NODE 0 (NODE 0 (LEAF 4) (LEAF 5)) (LEAF 6))
                  (NODE 0 (LEAF 8) (LEAF 9))
        t4 = ListNODE
                    [ ListNODE [ ListLEAF [1,2,3],ListLEAF [4,5],ListNODE([ListLEAF [6], ListLEAF []]) ],
                    ListNODE [],
                    ListLEAF [7,8],
                    ListNODE [ListLEAF [], ListLEAF []] ]  

                    
    print (merge2 [1,2,3,4,5,6] [4,5,6,7,8,9])
    print (merge2 [] [3,2,1])
    print (merge2 [1,2] [0,10,12])
    print (merge2 [1,3,3,5,5] [-1,2,4])
    print (merge2Tail [1,2,3,4,5,6] [4,5,6,7,8,9])
    print (merge2Tail [3,4,6,8] [1,3,4])
    print (merge2Tail [1,2] [0,10,12])
    print (merge2Tail [1,3,3,5,5] [-1,2,4])
    print (mergeN [[1,2],[10,12],[2,5,6,8,9]])
    print (mergeN [[3,4], [-3,-2,-1],[1,2,5,8,9]])
    print (getInRange 3 10 [1,2,3,4,5,6,7,8,9,10,11])
    print (getInRange (-5) 5 [-10,-5,0,5,10])
    print (getInRange (-1) 1 [-2,2,3,4,5])
    print (countInRange 10 10 [[1,2,3,4],[5,6,7,8,9],[10,11]])
    print (addLengths (FOOT 3) (FOOT 5))
    print (addAllLengths [[YARD 2, FOOT 1], [YARD 1, FOOT 2, INCH 10],[YARD 3]])
    print (sumTree t1)
    print (createSumTree t3)
    print (foldListTree (+) 0 t4)