module PT3
import StdEnv

//REMEMBER: PLAGIARISM IS ZERO!

//Please write your neptun code here: ZLWD2B

//Divide problem into multiple functions.


/*
You are getting two integers as an input. Given an integer n and k, count the total number of digit 
k appearing in all non-negative integers less than or equal to n. If k appears more than one times in number,
still count as 1. 

Example: n=13, k=1
All non-negative integers less than or equal to 13 are: 1,2,3,4,5,6,7,8,9,10,11,12,13 -> from these numbers we
encounter 1 in 1,10,11,12,13, so output should be 5. For 11, we are counting 1 one times.
*/

toList :: Int -> [Int]
toList 0 = [0]
toList x
| x < 10 = [x]
=  toList (x/10) ++ [x rem 10] 

countDigitK :: Int Int -> Int 
countDigitK n k = length [el \\ el <- [1..n] | (el == k) || isMember k (toList el)]


//Start = countDigitK 5 1 //5
//Start = countDigitK 0 9 // 0
//Start = countDigitK 100 5 // 19 









