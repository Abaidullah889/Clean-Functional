module cons
import StdEnv

//HOFs , list comprenhension , list recursion , revise recursion with examples
// map , filter , foldl/r , takewhile/dropwhile

//[1,2,3,4,5]
//[[1,2],[4],[8]]

// 9. Reverse every sublist of a list.


revsub3 :: [[Int]] ->  [[Int]]
revsub3 list = [reverse a \\ a<-list]


//Start = revsub3 [[1,2,3],[5,6],[],[7,8,9,10]] // [[3,2,1],[6,5],[],[10,9,8,7]]


// 19. Replicate n>0 times the element of a list e.g. n=3 [3..6] ->
// [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]

helpRepeat :: Int Int -> [Int]
//helpRepeat 0 _ = []
//helpRepeat n x = [x] ++ helpRepeat (n-1) x
helpRepeat n x = [x \\ y<-[1..n] ]

// [x] ++ helpRepaet n-1 x
// [x] ++ [x] ++ [] = [x,x]

// 


rep :: Int [Int] -> [[Int]]
//rep _ [] = []
//rep n [x:xs] = [helpRepeat n x] ++ rep n xs
//2nd way
//rep n list = [(repeatn n a) \\ a<-list ]
//3rd way
rep n list = map (helpRepeat n) list


//Start = rep 3 [3..6]



// 22. Using notempty, eliminate the empty lists from a list of lists. 
// [[1,2,3],[],[3,4,5],[2,2],[],[],[]] -> [[1,2,3], [3,4,5], [2,2]]

// filter
notempty :: [Int] -> Bool
notempty x = not (x == [])


f8 :: [[Int]] -> [[Int]]
f8 list = filter (notempty) list


f8 list = [x \\ x <- list | notempty x]


empty :: [[Int]] -> [[Int]]
empty [] = []
empty [x:xs] 
| notempty x = [x: empty xs]
= empty xs

//f8 :: [[Int]] -> [[Int]]

//Start = f8 [[1,2,3],[],[3,4,5],[2,2],[],[],[]] // [[1,2,3],[3,4,5],[2,2]]


/*9.
Given list of tuples and an integer value representing the 'pass_marks',
each tuple represents a student (name,marks), write a function which
groups the students into two categories based on their marks obtained in a test.
The function should return a tuple containing the list of the students
who failed, and the list of the students who passed.
Example:
List: [("Ramesh",23), ("Vivek",40), ("Harsh",88), ("Mohammad",60)], pass_marks: 30
Output: ([("Ramesh",23)],[("Vivek",40), ("Harsh",88), ("Mohammad",60)])
--failed-- -------------passed------------------------
'Ramesh' failed as his marks 23 are less than the given number 30, all others passed.
*/
group_by_marks :: [(String,Int)] Int -> ([(String,Int)],[(String,Int)])
group_by_marks list n = ([(a,b) \\ (a,b)<-list | b<n],[(a,b) \\ (a,b)<-list | b>=n])


// group_by_marks :: [(String,Int)] Int -> ([(String,Int)],[(String,Int)])
// group_by_marks list n = ((filter (\(a,b) = b < n) list),(filter (\(a,b) = b >= n) list))


// try eith recursion

//Start = group_by_marks [("Ramesh",23), ("Vivek",40), ("Harsh",88), ("Mohammad",60)] 30
// ([("Ramesh",23)],[("Vivek",40),("Harsh",88),("Mohammad",60)])
//Start = group_by_marks [("Ramesh",50),("Vivek",20),("Harsh",10),("Abdullah",90),("Mohammed",30),("Ahmed",0),("Othman",70)] 50
// ([("Vivek",20),("Harsh",10),("Mohammed",30),("Ahmed",0)],[("Abdullah",90),("Othman",70)])
//Start = group_by_marks [] 1 // ([],[])


// 23. Compute the sum of the sublists using foldr [[1,2,3], [3,4,5], [2,2]] -> [6, 12, 4]
//f9 :: [[Int]] -> [Int]

ff9 :: [Int] -> Int
ff9 list =  foldr (+) 0 list

//Start = ff9 [1..5]

//f9 list = map (foldr (+) 0) list  // partial parameterization !!!
//f9 list = [ff9 x \\ x<-list]
//f9 x = map sum x

//Start = f9 [[1,2,3], [3,4,5], [2,2]]




// 24. Write a function that keeps the integers of a list up to the first 0 encounterred 
// and then divides by 2 every element [1, 2, -2, 3, 0, -4] -> [0, 1, -1, 1]
// hints: use takeWhile then map
f3 :: [Int] -> [Int]
f3 list = map (\x = x/2) (takeWhile (\ x = x <> 0 ) list)
//Start = f3 [1, 2, -2, 3, 0, -4] // [0, 1, -1, 1]



// 25. Insert the sum of elements of the sublist as last element in every sublist of a list.
insLast :: [Int] -> [Int] 
insLast list = list ++ [sum list] 

insSum :: [[Int]] -> [[Int]]
insSum lists = map insLast lists

insSum2 :: [[Int]] -> [[Int]]
insSum2 lists = map (\x = x ++ [sum x]) lists

//Start = insSum [[1,2], [3,4,5], [6,5,9,7], [], [8]] // [[1,2,3],[3,4,5,12],[6,5,9,7,27],[0],[8,8]]
