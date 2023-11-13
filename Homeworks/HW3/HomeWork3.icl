
module HomeWork3
import StdEnv


//REMEMBER: PLAGIARISM IS ZERO!

//Please write your NAME AND NEPTUN code here: ZLWD2B 

/*
Your submission should not have any errors when running the code.

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add 
your tests as well. Don't change the given function signatures, however, you can add as many functions as 
you wish, just make sure to name them appropriately (if function squares the number, call it 'square',
'second_power', etc. and not 'f' or 'g'). The same goes for variable names. 

Make sure that you comment all 'Start'-s before submitting the code.

*/

/*
EXERCISE 1:

You are given one list of integers and one integer. You have to implement three operations:
 
1)Search - should give index of that element if it is in the list, if it is not in given list return -1
 
2)Erase - should erase that element from the list and give the index of deleted element if operation was successful, 
if it was not successful (for example that number was not in the list) it should return -1

3)Add - should add element to a sorted list so that the list will remain sorted after it, if the
insertion was successful return at what index was it inserted, if not return -1. Add operation won't be successful
if the element was already in the list. YOU CAN'T USE BUILTIN FUNCTION SORT !

Remember that given list is sorted and it should remain sorted !

*/

//Operation is given as a string:
   
   
   
search :: [Int] Int -> Int
search lst x 
|((isMember x lst) == False) = -1
| x == (hd (lst)) = 0 
= 1 + search (tl(lst)) x 



eraseAt :: [Int] Int -> [Int]
eraseAt [] _ = []
eraseAt [x:xs] t 
| t == x = eraseAt xs t
= [x:eraseAt xs t]




erase :: [Int] Int -> Int
erase lst x
|(isMember x lst == True) && (length lst <> length (eraseAt lst x)) = search lst x
= -1



insert_index :: [Int] Int -> Int
insert_index [h:t] y
| y <= h = 0
= 1 + (insert_index t y)


add :: [Int] Int -> Int
add lst x 
| isMember x lst = -1
| (length lst <> length z) = search z x

where z = insertAt (insert_index lst x) x lst





operations :: String [Int] Int -> Int
operations "search" list x = search list x
operations "add" list x = add list x
operations "erase" list x = erase list x


//Start = operations "search" [10, 20, 40, 50, 80] 80//4 --> index of 80 is 4
//Start = operations "search" [10, 20, 40, 50, 80] 25//-1 --> 25 is not in the list
//Start = operations "add" [10, 20, 40, 50, 80] 80//-1 --> 80 was already in list, so operation was unsuccessful
//Start = operations "add" [10, 20, 40, 50, 80] 25//2 --> new list: [10,20,25,40,50,80] --> index of 25 is 2
//Start = operations "add" [10, 20, 40, 50, 80] -100//0 --> -100 was inserted as first element 
//Start = operations "erase" [10, 20, 40, 50, 80] 80//4 --> index of 80 was 4, it was deleted successfully
//Start = operations "erase" [10, 20, 40, 50, 80] 25//-1 --> couldn't delete 25 cause it is not element of the list




/*
EXERCISE 2:
You are given a list of integers as an input. Remove those numbers from the list, that has the frequency 3.

Example:
In this list [1,1,1,2,2,3,3,3,4,4,4,4,5,5,5,6,7,7,7,7,7,7,7] 1,3 and 5 have frequency three, so they should be
removed from list. Output: [2,2,4,4,4,4,6,7,7,7,7,7,7,7]

*/


//Start = removeDup [1,1,1,2,2,3,3,3,4,4,4,4,5,5,5,6,7,7,7,7,7,7,7]


freq :: Int [Int] -> Int
freq x [h:t]
|(length t == 0 && x == h) = 1
|(length t == 0 && x <> h) = 0
| x == h = 1 + freq x t
= freq x t

freq_list :: [Int] [Int] -> [Int]
freq_list [] ori = []
freq_list [dh:dt] ori  
|x == 3 = [dh:freq_list dt ori]
= freq_list dt ori 
where x = freq dh ori

 

remove3Times::  Int Int [Int] -> [Int]
remove3Times 1 y lst = removeMember y lst
remove3Times x y lst = remove3Times (x-1) y (removeMember y lst)

 
 
helper:: [Int] [Int] -> [Int]
helper [] ori = ori
helper [x:xs] ori = helper xs (remove3Times 3 x ori)

removeIfThree :: [Int] -> [Int]
removeIfThree [] = []
removeIfThree lst = helper (freq_list (removeDup lst) lst) lst

   
  

//Start = removeIfThree [1,1,1,2,2,3,3,3,4,4,4,4,5,5,5,6,7,7,7,7,7,7,7]//[2,2,4,4,4,4,6,7,7,7,7,7,7,7]
//Start = removeIfThree []//[]
//Start = removeIfThree [11,11,22,22,22,33,33,33,33]//[11,11,33,33,33,33]
//Start = removeIfThree [11,11,22,22,5,22,33,5,33,5,33,33]//[11,11,33,33,33,33]




