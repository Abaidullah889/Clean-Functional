

Other standard functions on integer numbers include:

//abs the absolute value of a number
//sign -1 for negative numbers, 0 for zero, 1 for positive numbers
//gcd the greatest common divisor of two numbers
//^ raising a number to a power

Some standard functions on real numbers are:

// min and mix also take real numbers as an input
//sqrt the square root function
//sin the sine function
//ln the natural logarithm
//exp the exponential function (e-to-the-power-of)


Predefined functions on list:
//length determines the length of a list
//culates the sum of a list of whole numbers.
//reverse will return the reverse list.

//The operator ++ concatenates two lists. For example,
//Start = [1,2] ++ [3,4,5]
//will show the list [1,2,3,4,5].

//The function take operates on a number and a list. 
//If the number is n, the function will return the first n elements of the list. 
//For example, take 3[2..10] returns the list [2,3,4].

//The function take operates on a number and a list.
//If the number is n, the function will return the elements of the list after n. 
//For example, drop 3[2..10] returns the list [5,6,7,8,9,10].

//The function hd returns the first element of a list (its `head'),
//head :: [int] -> int
//head x = (hd) x
//head [x:xs] = hd[x:xs]
//Start = head [1,2,3,4,5] 


//while the function tl returns all but the first element (the `tail' of the list).
//tail :: [int] -> [int]
//tail x = (tl) x
//tail [x:xs] = tl[x:xs]
//Start = tail [1,2,3,4,5] 

// init will return the list of all elements except the last one.
// For example: [1,2,3,4,5] -> [1,2,3,4]

// last will return the last element of the list.
// For example :: last [1,2,3,4,5]-last>5


Predefined functions on functions:
//Start = map sqrt [1.0,2.0,3.0,4.0]
//Start = map isEven [1..8]


