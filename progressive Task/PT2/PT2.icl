module PT2
import StdEnv

/* Your neptun code :   ZLWD2B     */

// Copied tasks are marked 0 for everyone involved
// you can get partial points!

/* 
Implement a function collatzLength that calculates the length of the Collatz sequence for a given 
positive integer n. The Collatz sequence for a given n is defined as follows:
- If n is even, divide it by 2.
- If n is odd, multiply it by 3 and add 1.
- Repeat the process until n becomes 1.

The length of the sequence is the total number of steps taken to reach 1.

For example:
- `collatzLength 6` should return `8` because the sequence is `6, 3, 10, 5, 16, 8, 4, 2, 1`.
- `collatzLength 11` should return `15` because the sequence is 
`11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1`.

Your function should handle positive integers as input. If your input is negative, abort this message:
"The input is negative"
*/


collatz :: Int -> Int
collatz x
| isEven(x) = z
= y 
where
	z = x/2
	y = (x*3)+1


collatzLength :: Int -> Int
collatzLength x
| x == 0 = 0
| x == 1 = 1
| x < 0 = abort " The input is negative "
= 1 + collatzLength y
where y = collatz x


//Start = collatzLength 6//9
//Start = collatzLength 11//15
//Start = collatzLength 1//1
//Start = collatzLength 0//0
//Start = collatzLength -3//"The input is negative"






























