{-Haskell is available for Windows, Mac, and Linux. Here's the download page: https://www.haskell.org/downloads/.
We will be using the HUnit unit testing package in CptS 355.

Example of using the HUnit unit test framework.  See  http://hackage.haskell.org/package/HUnit for additional documentation.
To run the tests type "runTestTT tests" at the Haskell prompt.  -}

module HW2SampleTests
    where

import Test.HUnit
import Data.Char
import HW2

{- Two useful functions in the HUnit package are assertEqual and assertBool.
The arguments to 'assertEqual' are:
      a descriptive string
      the expected value
      the value being tested
The arguments to 'assertBool' are:
      a descriptive string
      the boolean value being tested
-}

p1a_test1 = TestCase (assertEqual "merge2 [2,5,6,8,9] [1,3,4,5,7,8,10]" [1,2,3,4,5,5,6,7,8,8,9,10]  (merge2 [2,5,6,8,9] [1,3,4,5,7,8,10]) )
p1a_test2 = TestCase (assertEqual "merge2 [] [1,2,3]" [1,2,3]  (merge2 [] [1,2,3]) )
p1a_test3 = TestCase (assertEqual "merge2 [1,2,5] [0,11,12]" [0,1,2,5,11,12]  (merge2 [1,2,5] [0,11,12]) )

p1b_test1 = TestCase (assertEqual "merge2Tail [2,5,6,8,9] [1,3,4,5,7,8,10]" [1,2,3,4,5,5,6,7,8,8,9,10]  (merge2Tail [2,5,6,8,9] [1,3,4,5,7,8,10]) )
p1b_test2 = TestCase (assertEqual "merge2Tail [3,4,6,8] [1,3,4]" [1,3,3,4,4,6,8]  (merge2Tail [3,4,6,8] [1,3,4]) )
p1b_test3 = TestCase (assertEqual "merge2Tail [1,2,3,5,10] [10]" [1,2,3,5,10,10]  (merge2Tail [1,2,3,5,10] [10]) )

p1c_test1 = TestCase (assertEqual "mergeN [[3,4],[-3,-2,-1],[1,2,5,8,9]]" [-3,-2,-1,1,2,3,4,5,8,9]  (mergeN [[3,4],[-3,-2,-1],[1,2,5,8,9]]) )
p1c_test2 = TestCase (assertEqual "mergeN [[3,4,5,6,7],[-1],[1,2,5,8,9]]" [-1,1,2,3,4,5,5,6,7,8,9]  (mergeN [[3,4,5,6,7],[-1],[1,2,5,8,9]]) )
p1c_test3 = TestCase (assertEqual "mergeN [[1,2],[],[1,2,5,8,9]]" [1,1,2,2,5,8,9]  (mergeN [[1,2],[],[1,2,5,8,9]]) )

p2a_test1 = TestCase (assertEqual "getInRange (-5) 5 [10,5,0,1,2,-5,-10]" [0,1,2]  (getInRange (-5) 5 [10,5,0,1,2,-5,-10]) )
p2a_test2 = TestCase (assertEqual "getInRange (-1) 1 [-2,2,3,4,5]" [] (getInRange (-1) 1 [-2,2,3,4,5]) )
p2a_test3 = TestCase (assertEqual "getInRange (0) 5 [1,2,0,3,-5,-10]" [1,2,3]  (getInRange (0) 5 [1,2,0,3,-5,-10]) )
p2a_test4 = TestCase (assertEqual "getInRange (-10) 10 [-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,2,3,10,11]" [-9,-8,-7,-6,-5,-4,-3,-2,-1,0,2,3] (getInRange (-10) 10 [-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,2,3,10,11]) )

p2b_test1 = TestCase (assertEqual "countInRange 3 10 [[1,2,3,4],[5,6,7,8,9],[10,11]]" 6 (countInRange 3 10 [[1,2,3,4],[5,6,7,8,9],[10,11]]) )
p2b_test2 = TestCase (assertEqual "countInRange (-5) 5 [[-10,-5,-4],[0,4,5],[],[10]]" 3 (countInRange (-5) 5 [[-10,-5,-4],[0,4,5],[],[10]]) )
p2b_test3 = TestCase (assertEqual "countInRange 10 10 [[1,2,3,4],[5,6,7,8,9],[10,11,12,13,14,15]]" 0 (countInRange 10 10 [[1,2,3,4],[5,6,7,8,9],[10,11,12,13,14,15]]) )
p2b_test4 = TestCase (assertEqual "countInRange (-1) 5 [[],[0,2,3,4,5],[],[10]]" 4 (countInRange (-1) 5 [[],[0,2,3,4,5],[],[10]]) )

p3a_test1 = TestCase (assertEqual "addLengths (FOOT 2) (INCH 5)" (INCH 29) (addLengths (FOOT 2) (INCH 5)) )
p3a_test2 = TestCase (assertEqual "addLengths (YARD 3) (INCH (-3))"  (INCH 105) (addLengths (YARD 3) (INCH (-3))) )
p3a_test3 = TestCase (assertEqual "addLengths (INCH 20) (INCH 5)" (INCH 25) (addLengths (INCH 20) (INCH 5)) )
p3a_test4 = TestCase (assertEqual "addLengths (YARD 3) (INCH (-8))"  (INCH 100) (addLengths (YARD 3) (INCH (-8))) )

p3b_test1 = TestCase (assertEqual "addAllLengths [[YARD 2, FOOT 1], [YARD 1, FOOT 2, INCH 10],[YARD 3]]" (INCH 262) (addAllLengths [[YARD 2, FOOT 1], [YARD 1, FOOT 2, INCH 10],[YARD 3]]) )
p3b_test2 = TestCase (assertEqual "addAllLengths [[YARD 2, FOOT 1], [YARD 1, FOOT 2, INCH 10],[YARD 3]]" (INCH 262) (addAllLengths [[YARD 2, FOOT 1], [YARD 1, FOOT 2, INCH 10],[YARD 3]]) )
p3b_test3 = TestCase (assertEqual "addAllLengths [[YARD 2], [YARD 1, INCH 10],[YARD 10]]" (INCH 478) (addAllLengths [[YARD 2], [YARD 1, INCH 10],[YARD 10]]) )
p3b_test4 = TestCase (assertEqual "addAllLengths [[],[YARD 3]]" (INCH 108) (addAllLengths [[],[YARD 3]]) )

p4a_test1 = TestCase (assertEqual ("sumTree "++(show t1)) 32 (sumTree t1) )
t1_output = NODE 32 (NODE 15 (NODE 9 (LEAF 4) (LEAF 5)) (LEAF 6)) (NODE 17 (LEAF 8) (LEAF 9))
p4a_test2 = TestCase (assertEqual ("sumTree "++(show t2)) 63 (sumTree t2) )
t2_output = NODE 9 (NODE 7 (NODE 1 (LEAF 0) (LEAF 1)) (LEAF 6)) (NODE 2 (LEAF 1) (LEAF 1))
p4a_test3 = TestCase (assertEqual ("sumTree "++(show t3)) 32 (sumTree t3) )
t3_output = NODE 19 (NODE 10 (NODE 4 (LEAF 4) (LEAF 0)) (LEAF 6)) (NODE 9 (LEAF 0) (LEAF 9))

p4b_test1 = TestCase (assertEqual ("createSumTree "++ (show t1)) (t1_output) (createSumTree t1) )
p4b_test2 = TestCase (assertEqual ("createSumTree "++ (show t2_test)) (t2_output) (createSumTree t2_test) )
p4b_test3 = TestCase (assertEqual ("createSumTree "++ (show t3_test)) (t3_output) (createSumTree t3_test) )

p5_test1 = TestCase (assertEqual ("foldListTree (+) 0 "++ (show t4)) 36 (foldListTree (+) 0 t4 ) )
p5_test2 = TestCase (assertEqual ("foldListTree (++) \"\" "++ (show t5)) "School-of-Electrical-Engineering-and-Computer-Science-WSU" (foldListTree (++) "" t5) )
p5_test3 = TestCase (assertEqual ("foldListTree (+) 0 "++ (show treeList1test)) 30 (foldListTree (+) 0 treeList1test))
p5_test4 = TestCase (assertEqual ("foldListTree (+) 0 "++ (show treeList2test)) 32 (foldListTree (+) 0 treeList2test))

-- Sample Tree Integer examples given in the assignment prompt; make sure to provide your own tree examples for both tree data types
-- Your trees should have minimum 3 levels.
t1 = NODE 1
         (NODE 2 (NODE 3 (LEAF 4) (LEAF 5)) (LEAF 6))
         (NODE 7 (LEAF 8) (LEAF 9))
t2 = NODE 0
          (NODE 0 (LEAF 4) (NODE 0 (LEAF 8) (LEAF 9)))
          (NODE 0 (NODE 0 (LEAF 10) (NODE 0 (LEAF 12) (LEAF 13))) (LEAF 7))

t3 = NODE 0 (NODE 0 (NODE 0 (LEAF 4) (LEAF 5)) (LEAF 6))
                (NODE 0 (LEAF 8) (LEAF 9))

t3_test = NODE 0 (NODE 0 (NODE 0 (LEAF 4) (LEAF 0)) (LEAF 6))
                (NODE 0 (LEAF 0) (LEAF 9))

t2_test = NODE 0 (NODE 0 (NODE 0 (LEAF 0) (LEAF 1)) (LEAF 6))
                (NODE 0 (LEAF 1) (LEAF 1))
t4 = ListNODE
 [ ListNODE [ ListLEAF [1,2,3],ListLEAF [4,5],ListNODE([ListLEAF [6], ListLEAF []]) ],
   ListNODE [],
   ListLEAF [7,8],
   ListNODE [ListLEAF [], ListLEAF []] ]

treeList1test = ListNODE
 [ ListNODE [ ListLEAF [1,2,3],ListLEAF [4,5],ListNODE([ListLEAF [6], ListLEAF []]) ],
   ListNODE [],
   ListLEAF [7],
   ListNODE [ListLEAF [], ListLEAF [2]] ]  

treeList2test = ListNODE
 [ ListNODE [ ListLEAF [2],ListLEAF [3,5],ListNODE([ListLEAF [6], ListLEAF [2]]) ],
   ListNODE [],
   ListLEAF [2,7],
   ListNODE [ListLEAF [1], ListLEAF [2,2]] ]  
l1 = ListLEAF ["School","-","of","-","Electrical"]
l2 = ListLEAF ["-","Engineering","-"]
l3 = ListLEAF ["and","-","Computer","-"]
l4 = ListLEAF ["Science"]
l5 = ListLEAF ["-WSU"]
n1 = ListNODE [l1,l2]
n2 = ListNODE [n1,l3]
t5 = ListNODE [n2,l4,l5]

tests = TestList [ TestLabel "Problem 1a - test1 " p1a_test1,
                   TestLabel "Problem 1a - test2 " p1a_test2,
                   TestLabel "Problem 1a - test3 " p1a_test3,
                   TestLabel "Problem 1b - test1 " p1b_test1,
                   TestLabel "Problem 1b - test2 " p1b_test2,
                   TestLabel "Problem 1b - test3 " p1b_test3,                  
                   TestLabel "Problem 1c - test1 " p1c_test1,
                   TestLabel "Problem 1c - test2 " p1c_test2,
                   TestLabel "Problem 1c - test3 " p1c_test3,
                   TestLabel "Problem 2a - test1 " p2a_test1,
                   TestLabel "Problem 2a - test2 " p2a_test2,
                   TestLabel "Problem 2a - test3 " p2a_test3,
                   TestLabel "Problem 2a - test4 " p2a_test4,
                   TestLabel "Problem 2b - test1 " p2b_test1,
                   TestLabel "Problem 2b - test2 " p2b_test2,
                   TestLabel "Problem 2b - test3 " p2b_test3,
                   TestLabel "Problem 2b - test4 " p2b_test4,
                   TestLabel "Problem 3a - test1 " p3a_test1,
                   TestLabel "Problem 3a - test2 " p3a_test2,
                   TestLabel "Problem 3a - test3 " p3a_test3,
                   TestLabel "Problem 3a - test4 " p3a_test4,
                   TestLabel "Problem 3b - test1 " p3b_test1,
                   TestLabel "Problem 3b - test1 " p3b_test1,
                   TestLabel "Problem 3b - test2 " p3b_test2,
                   TestLabel "Problem 3b - test3 " p3b_test3,
                   TestLabel "Problem 3b - test4 " p3b_test4,
                   TestLabel "Problem 4a - test1 " p4a_test1,
                   TestLabel "Problem 4a - test2 " p4a_test2,
                   TestLabel "Problem 4a - test3 " p4a_test3,
                   TestLabel "Problem 4b - test1 " p4b_test1,
                   TestLabel "Problem 4b - test2 " p4b_test2,
                   TestLabel "Problem 4b - test3 " p4b_test3,
                   TestLabel "Problem 5 - test1 " p5_test1,
                   TestLabel "Problem 5 - test2 " p5_test2,
                   TestLabel "Problem 5 - test3 " p5_test3,
                   TestLabel "Problem 5 - test4 " p5_test4
                 ]


-- shortcut to run the tests
run = runTestTT  tests
