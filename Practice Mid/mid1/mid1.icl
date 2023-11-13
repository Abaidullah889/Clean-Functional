module mid1
import StdEnv


remElemAt :: Int [Int] -> [Int]
remElemAt n lst = take n lst ++ drop (n+1) lst

remElemAt1 :: Int [Int] -> [Int]
remElemAt1 n lst = removeAt n lst

remElemAt2 :: Int [Int] -> [Int]
remElemAt2 n lst = [el \\ el <- lst & i <- [0..length(lst)] | n <> i] // important one , we can use this for indexing.


elemTill :: [Int] [Int] -> [Int]
elemTill first sec = [el \\ el <- first & i <- sec]	// it will display till the length of the sec array.

//Start = elemTill [1,2,-2,3,5,0,-4][1..8] 

elemTill2 :: [Int][Int] -> [Int]					// it will display all till it find the number that is equal to the length of sec array.
elemTill2 [] lst = []
elemTill2 [x:xs] lst
| x == y = []
= [x : elemTill2 xs lst]

where y = length lst

//Start = elemTill2 [1,2,-2,3,5,0,-4][1..5]

rep :: Int Int -> [Int]
rep 0 _ = []
rep n num = [num : rep (n-1) num]

//Start = rep 3 7

rep2 :: Int [Int] -> [[Int]]
rep2 n [] = []
rep2 n [x:xs] = [rep n x : rep2 n xs] 

//Start = rep2 3 [3..6]

rep21 :: Int [Int] -> [[Int]]
rep21 n lst = map ((rep)3) lst 

//Start = rep21 3 [3..6]

// There is also a built-in funcction for repeatiton of a number.

//Start = repeatn 3 7

dobPos :: [Int] -> [Int]				// this will compute the double of a positive number in the list.
dobPos lst = [el*2 \\ el <-lst | el > 0]

//Start = dobPos [1,2,-2,3,-4]

filsmall :: Int [Int] -> [Int]    // this will filter all the elements that are smaller than n.
filsmall n lst = [el \\ el <- lst | el > n]

filsmall1 :: Int [Int] -> [Int]
filsmall1 n lst = filter ((<)n) lst 

//Start = filsmall1 3 [1,2,3,4,5,6,7,8]

notempty :: [Int] -> Bool				//it will eliminate all the empty lists from the double list.
notempty lst = length lst <> length [] 

elimEmpt1 :: [[Int]] -> [[Int]]
elimEmpt1 lst = [el \\ el <-lst | (notempty el == True)]

elimEmpty2 :: [[Int]] -> [[Int]]
elimEmpty2 lst = filter notempty lst 

//Start = elimEmpty2 [[1,2,3],[],[3,4,5],[2,2],[],[],[]]



// Write a function that checks if each element in the list appears even time or not?

freq :: [Int] Int -> Int
freq lst n  = length (filter ((==) n) lst )

freqs :: [Int] Int -> Int
freqs lst n = length [x \\ x <- lst | x==n]

freq1 :: [Int] Int -> Bool
freq1 lst n = isEven (length (filter ((==) n) lst ))

checkEven :: [Int] -> Bool
checkEven lst = and (map (freq1 lst) lst) 

//Start = checkEven [1,1,2,2,2,2,3,5,3,5]


occurMost :: [Int] -> Int // the most occurence
occurMost lst = last (sort (map (\x = freq lst x) lst))

//Start = occurMost [1,1,2,2,2,2,3,5,3,5]

occurMost1 :: [Int] -> Int 			// it will check which element occurs most.
occurMost1 lst = fst (hd (filter (\tuples = (snd tuples) == maxCnt) tuples))

where 
	tuples = [(el,(freqs lst el)) \\ el<-(removeDup lst)]
 	maxCnt = maxList [cnt \\ (el,cnt) <- tuples]

//Start = occurMost1 [1,1,2,2,2,2,3,5,3,5]



avgTuple:: (Int,Int) -> Real
avgTuple tuple = toReal ((fst tuple) + (snd tuple)) / 2.0
Start = avgTuple (10,5)




