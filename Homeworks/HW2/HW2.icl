module HW2
import StdEnv

//REMEMBER: PLAGIARISM IS ZERO!

//Please write your neptun code here: ZLWD2B

/*
Your submission should not have any errors when running the code.

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add 
your tests as well. Don't change the given function signatures, however, you can add as many functions as 
you wish, just make sure to name them appropriately (if function squares the number, call it 'square',
'second_power', etc. and not 'f' or 'g'). The same goes for variable names. 

Make sure that you comment all 'Start'-s before submitting the code.

//Divide problems into multiple functions to make it less complicated to solve!

*/


/*
EXERCISE 1:

Pell numbers are a sequence of integers that have many interesting mathematical properties. 
The Pell numbers are defined by the recurrence relation:

P(0)=0
P(1)=1
P(n)=2·P(n-1)+P(n-2)  for  n>1

The first few Pell numbers are:
0,1,2,5,12,29,70,169,408,985,…

You should define a function - sumOfPel, which will take a non-negative integer n 
and returns the sum of the first n Pell numbers.

Example:
sumOfPel 5 = 20 --> sum of first five pell numbers (0+1+2+5+12)

*/

Pel :: Int -> Int
Pel 0 = 0
Pel 1 = 1   
Pel x = 2 * Pel (x-1) + Pel (x-2)

sumOfPel :: Int -> Int
sumOfPel 0 = 0
sumOfPel x = Pel(x-1) + sumOfPel(x-1)

 
//Start = sumOfPel 0//0
//Start = sumOfPel 5//20
//Start = sumOfPel 9//696


/*
EXERCISE 2:

Check if n-th Fibonacci number is prime.
You can use the standard definition of Fibonacci numbers:
F(0)=0
F(1)=1
F(n)=F(n-1)+F(n-2)  for  n>1

The first few Fibonacci numbers are:
0,1,1,2,3,5,8,13,21,34,…

HINT: You should define isPrime function.

*/



isPrime :: Int Int -> Bool
isPrime 0 y = False
isPrime 1 y = False
isPrime 2 y = True
isPrime 3 y = True
isPrime x y
|(x rem y == 0) = False
| y >= (x/2) = True
= isPrime x z
where z = y+1

//Start = isPrime 46 2

Fib :: Int -> Int
Fib 0 = 0
Fib 1 = 1
Fib x = Fib(x-1)+Fib(x-2)


isFibonacciPrime :: Int -> Bool
isFibonacciPrime x = isPrime (Fib x) 2

//Start = isFibonacciPrime 8 //fib(8)=21 --> False, 21 is not Prime
//Start = isFibonacciPrime 7 //fib(8)=13 --> True, 13 is Prime
//Start = isFibonacciPrime 10 //fib(8)=55 --> False
//Start = isFibonacciPrime 5 //fib(5)=5 --> True
//Start = isFibonacciPrime 0//False














