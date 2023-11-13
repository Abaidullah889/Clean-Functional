module midtermm

import StdEnv

// Please fill the data required below.
//<Name Abaidullah Asif>
//<Neptun_code ZLWD2B>
//Functional Programming & mid-term-retake
//2021.September.14 
//This solution was submitted and prepared by <Name, Neptun_code> for the mid-term assignment of the Functional Programming course.
//I declare that this solution is my own work.
//I have not copied or used third party solutions.
//I have not passed my solution to my classmates, neither  made it public.
//Students� regulation of E�tv�s Lor�nd University (ELTE Regulations Vol. II. 74/C.) 
//states that as long as a student presents another student�s work - 
//or at least the significant part of it - as his/her own performance, it will count as a disciplinary fault. 
//The most serious consequence of a disciplinary fault can be dismissal of the student from the University.



/*1. List ends
 Given a list of lists, append to the end of every sublist 
 the sum and the length of the sublist
*/

append :: [[Int]] -> [[Int]]
append lst = [ (el ++ [sum el] ++ [length el]) \\ el <- lst]
//Start = append [[1..5],[1..4],[],[5,6]]  // [[1,2,3,4,5,15,5],[1,2,3,4,10,4],[0,0],[5,6,11,2]]
//Start = append [[(-1),(-2)..(-10)],[12],[5]]  // [[-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-55,10],[12,12,1],[5,5,1]]
//Start = append []  // []



/* 2. Fractions
 Given a list of real numbers, keep only the fraction part of the number
*/

do :: Real -> Real
do n
| toReal (toInt n) > n = (n - toReal ((toInt (n))-1))
= (n - toReal (toInt n)) 


fraction :: [Real] -> [Real]
fraction lst = [do el \\ el <- lst]
//Start = fraction [1.2,1.5,0.6] //[0.2,0.5,0.6]
//Start = fraction [1.25, 8.2115548896, 53.21,45.58,0.005] //[0.25,0.2115548896,0.21,0.58,0.00005]
//Start = fraction [] // []


/*3. Famous nums

 Given a list of integers, write a function which gets rid of the numbers that is occurring
 less than 5 times in the list.
*/

occur :: Int [Int] -> Int
occur n lst = length [el \\ el <- lst | el == n]

famousNum :: [Int] -> [Int]
famousNum lst = [el \\ el <- lst | occur el lst >= 5 ]


//Start = famousNum [1,1,1,1,1,1,2,3,4,4,4,4,5,5,5,5,5] // [1,1,1,1,1,1,5,5,5,5,5]
//Start = famousNum [] // []
//Start = famousNum [1,2,3,4,5,6,1,1,1,2,2,2,2,1,1,5,10,3] // [1,2,1,1,1,2,2,2,2,1,1]




/*4. Search
 
 Implement a search algorithm that searches through a list for Int n and returns the value in the list before n. 
 If there is no value, or the list is empty, return -1. e.g., findPrev 5 [1,2,3,4,5,6] should return 4, 
 while findPrev 5 [0, 10, 20, 30] returns -1.
*/

index :: Int [Int] -> Int
index n [x:xs]
| n == x = 0
= 1 + index n xs

findPrev :: Int [Int] -> Int
findPrev n [] = -1 
findPrev n lst
| n == hd lst = -1 
| not (isMember n lst) = -1
= lst !! (index n lst)-1


// findIndex ... 
//Start = findPrev 5 [1,2,3,4,5,6] // 4
//Start = findPrev 1 [1,2,3,4,5,6] // -1
//Start = findPrev 1 [] // -1 

 

/* 5. Symmetric difference 

 Given two lists of integer numbers , return a sorted list containing the symmetric difference of the two lists; 
 The symmetric difference of two lists A and B is the list (A - B) U (B - A); 
 where A - B is The difference of two lists  defined as follows:  
 The List A-B consists of elements that are in A but not in B.
 And (U) the union of two lists is a list containing all the elements of A and B without duplicates 
*/

dif :: [Int] [Int] -> [Int]
dif lst lst1 = [el \\ el <- lst | not(isMember el lst1)]

union :: [Int] [Int] -> [Int]
union lst lst1 = [a \\ a <- lst] ++ [b \\ b<-lst]


symmetricDif :: [Int] [Int] -> [Int]
symmetricDif lst lst1 = (dif lst lst1) ++ (dif lst1 lst)

//Start = symmetricDif  [1,2,3,4,5] [2,4,6] //  [1,3,5,6]
//Start = symmetricDif  [1..5] [1..10] // [6,7,8,9,10]
//Start = symmetricDif  [1..5] [] // [1,2,3,4,5]



/*6. Not N

 Given a list of integers and an integer N, 
 eliminate from the list elements that are positioned before N in the list and are not equal to N,
 then compute the biquadrate of the numbers left in the list.
*/


notN :: Int [Int] -> [Int]
notN n lst = [el ^ 4 \\ el <- lst & i <- [1..] | i >= ((index n lst)+1)]


//Start = notN 3 [1..5] // [81,256,625]
//Start = notN 0  [] // []
//Start = notN 6 [10,8..1] // [1296,256,16]




/* 7.  Gap2 continued 

 Given a list of numbers, return True if the  
 the difference between two consecutive elements is always 2
 otherwise return False
*/

helper :: [Int] -> Bool
helper [x,y] = y-x==2
helper [x,y:xs]
| y-x == 2 = helper [y:xs]
= False
gap2C :: [Int] -> Bool
gap2C [] = False
gap2C lst = helper lst
 
//Start = gap2C [1,3,5,7] // True
//Start = gap2C [1,3,5,7,9,11,13,15] // True
//Start = gap2C [1,5,8] // False
//Start = gap2C [] // False


/*9. CoPrimes
 Given 2 numbers, check if they are co-prime.
 Numbers are called co-prime if they do not have
 common divisor.
*/

isPrime :: Int -> Bool
isPrime n = length [el \\ el <- [1..n] | n rem el == 0] == 2

dPrimes :: Int Int -> Bool
dPrimes n m = length [el \\ el <- [1..n] | (isPrime el) && (n rem el == 0) && (m rem el == 0)] == 0

coPrimes :: Int Int -> Bool
coPrimes n m = dPrimes n m
//Start = coPrimes 12 9 // False
//Start = coPrimes 12 12 // False
//Start = coPrimes 12 13 // True
//Start = coPrimes 5 7 // True


