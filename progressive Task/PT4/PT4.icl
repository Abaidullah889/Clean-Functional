module PT4
import StdEnv

//REMEMBER: PLAGIARISM IS 0!

//WRITE YOUR NAME AND NEPTUN CODE HERE Abaidullah ZLWD2B:

//Task:
//Find the most frequent element in the list and output its frequency. NOTE: list can be of any type a!

occur :: a [a] -> Int | == a
occur n lst = length [el \\ el <- lst | el == n]


maxFreq :: [a] -> Int | == a
maxFreq lst = maxList [occur a lst \\ a <- lst]

//Start = maxFreq ['a', 'b', 'c', 'a', 'a']//3 --> most frequent element is 'a', frequency is 3
//Start = maxFreq [1, 1, 2, 2, 2, 2, 3, 4, 5, 5, 5]//4 --> most frequent element is 2, frequency is 4
//Start = maxFreq [True, False, False, False, True]//3 --> most frequent element is False, frequency is 3
//Start = maxFreq ["I", "will", "pass", "functional", "ha", "ha"]//2 --> most frequent element is "ha", frequency is 2
//Start = maxFreq [1.1, 2.2, 3.3, 3.3, 3.3, 3.3, 3.3]//5

