module PT1
import StdEnv

/* Your neptun code :  ZLWD2B      */

/* 1.
Write getLastIfPositive function which takes one integer as an input
and outputs string. If the given number is positive, it should
return last digit of this number, but converted to string. If the
number is zero, just return "0". If the number is negative, it 
should output: "Given number is negative".
*/

getLastIfPositive :: Int -> String
getLastIfPositive n
| n > 0 = toString (n rem 10)
| n < 0 = "Given number is negative"
= "0"
Start = getLastIfPositive 1984 //"4"
//Start = getLastIfPositive -2 //"Given number is negative"
//Start = getLastIfPositive 0 //"0"