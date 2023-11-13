module HW1
import StdEnv

//Please write your name and neptun code here: Abaidullah Asif    :: ZLWD2B

/*
Don't copy the others' work, otherwise, you won't get point for this homework.
	Changing the function and variable does not help. 
	
Your submission should not have any errors when running the code.

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You'll receive a total of 100 points when you successfully solve both problems, with 50 points awarded for each.

You should not delete anything from the given code, given test cases should stay the same, but you can add 
your tests as well. Don't change the given function signatures. 

Make sure that you comment all 'Start'-s before submitting the code.
		 
*/








/*
TASK 1:

Write a function which takes four parameters as an input - one integer, one real number, one boolean and 
one character. If corresponding character of that integer in the ASCII table is UPPERCASE letter 
 AND second digit after decimal point of given real is ODD
 AND given boolean value is FALSE
return MINIMUM of given real and int, otherwise return ASCII value of character given as an input


HINT: There are built-in functions min and max, they take two numbers, and return one, maximum or minimum 
      respectively.


For example: input is - 66 66.99 (isOdd 2) 'A'
Corresponding character of 66 is 'B' which is upper case letter, so first part of condition is fulfiled,
let's see second part, 66.99 - in this real number second digit after decimal point is 9 which is odd, so it 
is also fulfiled, and given boolean - (isOdd 2) is false, so every three part of condition is correct,
we should output minimum of 66 and 66.99 which is 66 !*/


f1 :: Int Real Bool Char -> Real
f1 i r b c 
| (i >= 65 && i <= 90) && (isOdd ((toInt(r*100.0) rem 10))) && b==False = min (toReal i) r
= toReal x
where x = toInt c


//Start = f1 66 66.99 (isOdd 2) 'A' // 66
//Start = f1 80 90.24 ((toInt 'C')<>67) 'F'// 70 --> 4 is not odd, so output is 70 which is ascii value for 'F'
//Start = f1 104 104.34 ((toLower 'B')=='B') '@'//64 --> 104 is ASCII code for 'h', which is lowercase, so 
											  //condition is not fulfiled and output is 64, ASCII value for '@'
//Start = f1 72 0.07 True '#' // 35 --> given boolean is True instead of False, so output is 35, ASCII value for '#'






/*
TASK 2:

Write a function which takes two characters as an input. Every character has corresponding ascii value (numeric).
So convert given characters to numbers, substract them, get absolute value of the difference and check if it is 
equal to the square root of their sum. If they are equal, convert them to string, concatinate them and output, 
if they are not equal print : "Given characters do not fulfil condition!"

For example: ASCII value for 'B' and 'N' - 66 and 78. [66-78]=12, 66+78=144, and root from it is 12 too. So output
is "BN". 'A' and 'B' don't fulfil that condition, so output is - "Given characters do not fulfil condition!"
*/



//ch2 :: Int -> Int
//ch2 x =                          toInt (sqrt (toReal(x)))
//Start = ch2 4

//ch2 :: Char Char -> Bool
//ch2 a b =(abs (toInt(a)-toInt(b))) == toInt (sqrt (toReal ((toInt(a) + toInt(b)))))

//Start = ch2 'B' 'N'




f2 :: Char Char -> String
f2 x y
| (abs (diff)) == toInt (sqrt (toReal sum)) = toString(x)+++toString(y)
= "Given characters do not fulfil condition!"
where
   a = toInt(x)
   b = toInt(y)
   sum = a + b
   diff = a - b  
   
//Start = f2 'B' 'N'//"BN"
//Start = f2 'i' '['//"i["
//Start = f2 '[' 'N'//"[N"
//Start = f2 'A' 'B'//"Given characters do not fulfil condition!"






