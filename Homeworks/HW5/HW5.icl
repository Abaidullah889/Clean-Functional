module HW5
import StdEnv


//REMEMBER: PLAGIARISM IS ZERO!

//Please write your NAME and neptun code here: Abaidullah Asif  ZLWD2B 


//TASK 1:

/*
You are given a list of characters. Output a string, which will contain same characters from the given list
on one line. 
Hint: "\n" is a new line character.
*/



helper :: [Char] [Char] -> [[Char]]
helper lst t = [(helper2 lst x) \\ x <- t]


helper2 :: [Char] Char -> [Char]
helper2 lst t = [el \\ el <- lst | el == t]


helper3 :: [Char] -> String
helper3 [] = ""
helper3 [x] = (toString x)
helper3 [x,y:xs]
| x == y = (toString x) +++ helper3 [y:xs]
= (toString x) +++ "\n" +++ helper3 [y:xs]


collectChars :: [Char] -> String
collectChars lst =  helper3 (flatten (helper lst (removeDup lst)))


//Start = collectChars []//""
//Start = collectChars ['a', 'a', 'a', 'b', 'b', 'c', 'c', 'c', 'c', 'c']
/* Output:
"aaa
bb
ccccc
"
*/
Start = collectChars ['*','*','@','#','*','#','#','@','#','#','@','*', '*', '*', '@', '#']
/* Output:
"******
@@@@
######
"
*/

//TASK 2:

/*
Your input is a list of characters. Write function that will pack the consecutive duplicates of the 
list's elements into sublists,meaning that if the list contains repeated elements they should be placed 
in separate sublists.
*/

separate :: [Char] -> [[Char]]
separate [] = []
separate [x:xs] = [(takeWhile ((==) x) [x:xs])] ++ (separate (dropWhile ((==)x) [x:xs]))


//Start = separate ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] // [['a','a','a','a'],['b'],['c','c'],['a','a'],['d'],['e','e','e','e']]
//Start = separate ['a','b','c','d'] // [['a'],['b'],['c'],['d']]
//Start = separate ['a','a','b','b','b','b','b','b','b','b','b','b','b','a','a'] //[['a','a'],['b','b','b','b','b','b','b','b','b','b','b'],['a','a']]
                           
                            
                            
                            
                            
