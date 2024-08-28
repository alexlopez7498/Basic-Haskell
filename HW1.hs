-- CptS 355 - Spring 2024 Assignment 1
-- Please include your name and the names of the students with whom you discussed any of the problems in this homework
-- Alex Lopez-Garcia, I discussed homework problems with peter L., Jack Q., and Chase L.
module HW1
     where

import Data.Char (toUpper)

-- HW1.hs
-- 1. exists
exists :: Eq t => t -> [t] -> Bool
exists x [] = False  -- this line checks if the list is empty or we made it to the end of the list and then it returns false
exists x (y:ys)
     | x /= y = exists x ys -- this line check if x is not equal to and then recursively uses the tail 
     | otherwise = True -- this line checks if they were equal to each other then it returns true

{- 1. part b
For exists we need to compare item in a list using == or /= with the Eq type it allows us to do that.
But if we didn't have it we would be able to use those operations which will give us errors.
-}

-- 2. listUnion
listUnion :: Eq a => [a] -> [a] -> [a]
listUnion [] [] = [] -- if both lists are empty then we return an empty list
listUnion (x:xs) [] = remvRepeats ((x:xs) ++ []) --if the second list is empty we just go into remv function which removes dupes
  where
     remvRepeats :: Eq a => [a] -> [a]
     remvRepeats [] = []
     remvRepeats (x:xs)
          | exists x xs = remvRepeats xs -- if the head of the list exists in the tail then we recursive go back into it without the head so we remove a dupe
          | otherwise = x : remvRepeats xs -- otherwise we append the value to the main list that we will return
listUnion [] (y:ys) = remvRepeats ([] ++ (y:ys)) --if the first list is empty we just go into remv function which removes dupes
  where
     remvRepeats :: Eq a => [a] -> [a]
     remvRepeats [] = []
     remvRepeats (x:xs)
          | exists x xs = remvRepeats xs -- if the head of the list exists in the tail then we recursive go back into it without the head so we remove a dupe
          | otherwise = x : remvRepeats xs -- otherwise we append the value to the main list that we will return
listUnion (x:xs) (y:ys) = remvRepeats ((x:xs) ++ (y:ys)) -- if both the list have items in it
  where
     remvRepeats :: Eq a => [a] -> [a]
     remvRepeats [] = []
     remvRepeats (x:xs)
          | exists x xs = remvRepeats xs -- if the head of the list exists in the tail then we recursive go back into it without the head so we remove a dupe
          | otherwise = x : remvRepeats xs -- otherwise we append the value to the main list that we will return

-- 3. replace
replace :: (Eq t1, Num t1) => t1 -> t2 -> [t2] -> [t2]
replace n value (y:ys)
     | n == 0 = value : ys -- when is n == 0 then we know where to replace 
     | n /= 0 && isEmpty ys = (y:ys) -- this check is when n is not equal to zero and the tail of the list is empty then we know we dont replace anything a return the orginial list
     | otherwise = y : replace (n - 1) value ys -- and if we are not at the right index then we recursively go into the function will n == 0
  where
     isEmpty :: [a] -> Bool -- helper function that checks if the a list is empty or not
     isEmpty [] = True
     isEmpty (x:xs) = False

-- 4. prereqFor
prereqFor :: Eq t => [(a, [t])] -> t -> [a]
prereqFor [] target = [] -- if the prereqsList is empty and we have a target then we just return a empty list
prereqFor prereqList target 
    | exists target prereqs = fst (head prereqList) : prereqFor (tail prereqList) target -- this checks if the target exists in the list inside the main list and if it is then we append the first element of the list into the main list that it will return
    | otherwise = prereqFor (tail prereqList) target -- otherwise we go no the next item into the list recursively
  where 
    prereqs = snd (head prereqList) -- we set the prereqs variable as the second element of the nested list which is a list

-- 5. isPalindrome
isPalindrome :: [Char] -> Bool
isPalindrome [] = True   -- if the list is empty we know its a palindrome
isPalindrome word
    | length word == 1 = True -- if the list is just 1 character long then its a palindrome
    | upperWord == reverse upperWord = True -- this checks the whole word with the reverse if it and if they're the same then we return true
    | otherwise = False --  and if its not then its false
  where 
     upperCaseWord :: [Char] -> [Char] -- helper function that makes the word all uppercase
     upperCaseWord [] = []
     upperCaseWord (x:xs)
          | x == ' ' = upperCaseWord xs -- this checks letter by letter if there's a space in the list and if there is then we just dont append it and recursively go back into the function with the tail
          | x /= toUpper x = toUpper x : upperCaseWord xs -- this checks if the character is uppercase and if its not then we append the uppercase of that letter
          | otherwise = x : upperCaseWord xs -- this otherwise appends it if its already uppercase
     upperWord = upperCaseWord word -- variable to hold the uppercase word
-- 6. groupSumtoN
groupSumtoN :: (Ord a, Num a) => a -> [a] -> [[a]]
groupSumtoN _ [] = [[]]
groupSumtoN total (x:xs)
  | x > total = [x] : groupSumtoN total xs -- if the head of the list is greater than the total then we just append a list which is just that number
  | otherwise = findSubList total [x] xs -- if its not greater then we go into the helper function
  where
    findSubList :: (Ord a, Num a) => a -> [a] -> [a] -> [[a]] -- this helper function finds the sublist
    findSubList total list [] = [reverse list] -- if the tail of the list empty then we know we made it to the end
    findSubList total list (y:ys) 
      | sum (y : list) <= total = findSubList total (y : list) ys -- check if the sum of the next character will because less than or equal to and if it is then we recursively go back into it with the same list adding the new character
      | otherwise = reverse list : findSubList total [y] ys -- otherwise we found the the whole subList so then we append it to findSubList

main :: IO ()
main = do
    let prereqsList = [ ("CptS122" , ["CptS121"]), -- making all the classes into a variable
                        ("CptS132" , ["CptS131"]),
                        ("CptS223" , ["CptS122", "MATH216"]),
                        ("CptS233" , ["CptS132", "MATH216"]),
                        ("CptS260" , ["CptS223", "CptS233"]),
                        ("CptS315" , ["CptS223", "CptS233"]),
                        ("CptS317" , ["CptS122", "CptS132", "MATH216"]),
                        ("CptS321" , ["CptS223", "CptS233"]),
                        ("CptS322" , ["CptS223","CptS233"]),
                        ("CptS350" , ["CptS223","CptS233", "CptS317"]),
                        ("CptS355" , ["CptS223"]),
                        ("CptS360" , ["CptS223","CptS260"]),
                        ("CptS370" , ["CptS233","CptS260"]),
                        ("CptS427" , ["CptS223","CptS360", "CptS370", "MATH216", "EE234"])
                      ]
    print (exists '8' "I have a Bed")
    print (listUnion [1,1,1,3,1] [1,21,3,3,4,5])
    print (listUnion [1,1,1,1,3,3,4,5,5] [1,1,1,1,1,4,5,6,7,7,7,7,7])
    print (listUnion "CptS355" "cpts322")
    print (replace 3 40 [1,2,3,4,5,6])
    print (isPalindrome "hel leeh")
    print (isPalindrome "Yreka Bakery")
    print (prereqFor prereqsList "CptS317")
    print (groupSumtoN 15 [1,2,3,4,5,6,7,8,9,10])
    print (groupSumtoN 1 [1,2,3,4,5,6,7,8,9,10])
    print (groupSumtoN 11 [1,2,3,4,5,6,7,8,9,10])
    print (groupSumtoN 100 [1,2,3,4,5,6,7,8,9,10])