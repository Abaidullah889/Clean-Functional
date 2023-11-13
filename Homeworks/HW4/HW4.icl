module HW4
import StdEnv

//REMEMBER: PLAGIARISM IS ZERO!

//Please write your NAME and neptun code here: ZLWD2B

//TASK 1
/*
Your input is list of lists of any type a. Your output should be list of tuples (x,y). Where x is each sublist
of input list, and y is its lenght's frequency, meaning, if this sublist has length 3, and there are
5 more sublists with length 3, then frequency shows 6.

Example:

Input: [["aaaa"],["a","a","a"],["a","a","a"],["a"],["a","a"],[""]]

There are 3 elements with length 1, 2 elements with the length 3, and 1 element with the length 2, so the 
output should be: -->

Output: [(["aaaa"],3),(["a","a","a"],2),(["a","a","a"],2),(["a"],3),(["a","a"],1),([""],3)]

*/

isValid :: [[a]] [a] -> [[a]]
isValid [] _ = []
isValid [x:xs] lst
| (length x == length lst) = [x:isValid xs lst]
= isValid xs lst  

freq :: [a] [[a]]-> Int
freq x y = length (isValid y x)

//Start = freq ["a","a","a"] [["aaaa"],["a","a","a"],["a","a","a","a"],["a"],["a","a","a","a","a"],["a","a"],["a","a","a","a","a"],[""]]


createTuples :: [[a]] -> [([a],Int)]
createTuples lst = map (\x = (x,freq x lst)) lst


myList = [["aaaa"],["a","a","a"],["a","a","a","a"],["a"],["a","a","a","a","a"],["a","a"],["a","a","a","a","a"],[""]]

//Start = createTuples myList//[(["aaaa"],3),(["a","a","a"],1),(["a","a","a","a"],1),(["a"],3),(["a","a","a","a","a"],2),(["a","a"],1),(["a","a","a","a","a"],2),([""],3)]

//Start = createTuples [[2],[6],[4],[7],[0],[5,6],[3,4],[5,4],[1,2,3,4,5,6]]//[([2],5),([6],5),([4],5),([7],5),([0],5),([5,6],3),([3,4],3),([5,4],3),([1,2,3,4,5,6],1)]

//Start = createTuples [['a'],['a','b'],['a','b','c']]//[(['a'],1),(['a','b'],1),(['a','b','c'],1)]




//TASK 2
/*
You are given one list of integers, every integer is >= 1 in the list.

1)Make sure that your list is sorted until you start changing it:
[4,3,5] -> [3,4,5]

2)Every element in the list should have same frequency as their value(3 should be 3 times in list).
[3,4,5] -> [3,3,3,4,4,4,4,5,5,5,5,5]

3)Take middle element for every value and change it with 100:
[3,3,3,4,4,4,4,5,5,5,5,5] -> [3,100,3,4,4,100,4,5,5,100,5,5] (middle for 4 elements is third element)

*/

rep :: Int Int -> [Int]
rep _ 0 = []
rep x y = [x] ++ rep x (y-1) 

helper :: [Int] -> [[Int]]
helper [] = []
helper [x:xs] = [(rep x x):helper xs]  										// I want to do it with map.

helper2 :: [[Int]] -> [[Int]]
helper2 [] = []
helper2 [x:xs] = [(updateAt (((length x) / 2)) 100 x)] ++ helper2 xs 

taskSecond :: [Int] -> [Int]
taskSecond [] = []
taskSecond x = flatten (helper2(helper (sort (removeDup x))))
 
 
taskSecond1 :: [Int] -> [[Int]]
taskSecond1 lst =  flatten (map (\x = updateAt ((length lst) / 2) 100 x) (map (\x = repeatn x x) (sort (removeDup lst))))

Start = taskSecond1 [4,3,5]//[3,100,3,4,4,100,4,5,5,100,5,5]
//Start = taskSecond [2,5,2,2,5,5,5,5,5,5]//[2,100,5,5,100,5,5]
//Start = taskSecond [25]//[25,25,25,25,25,25,25,25,25,25,25,25,100,25,25,25,25,25,25,25,25,25,25,25,25]
//Start = taskSecond [7,9,5,3]//[3,100,3,5,5,100,5,5,7,7,7,100,7,7,7,9,9,9,9,100,9,9,9,9]
//Start = taskSecond [] //[]
//Start = taskSecond [6,16,26]//[6,6,6,100,6,6,16,16,16,16,16,16,16,16,100,16,16,16,16,16,16,16,26,26,26,26,26,26,26,26,26,26,26,26,26,100,26,26,26,26,26,26,26,26,26,26,26,26]


