module HW9 2
import StdEnv

//Write your neptune code and name here: <	Abaidullah Asif	, ZLWD2B    >

//As promised this hw is very easy :)

//Got plagiarism checker, so now there's 100% guarantee to get 0 for copying :(


/*
--------------------------------------TASK 1 - 30 POINTS--------------------------------------------------------

We created type course and some records of that type (Programming, Analysis and so on). 
Your input is tree of Courses. Your output should be tree of booleans. 
For every parent node you should do this:
Go through every node in left subtree and sum up credits for every course in that tree, than do same for right
subtree. If sum of left is less than some of right, replace current Node's value with True, otherwise replace it
with False.


*/

:: Course = {name::String, credits:: Int}

Programming::Course
Programming = {name="Programming", credits =5}

Analysis::Course
Analysis = {name="Analysis", credits =4}

Relativity::Course
Relativity = {name="Relativity", credits=6}

Functional::Course
Functional = {name="Functional", credits=5}

Basic::Course
Basic = {name="Basic", credits=3}

Thermo_Dynamics::Course
Thermo_Dynamics = {name="Thermo_Dynamics", credits=4}

Astronomy::Course
Astronomy = {name="Astronomy", credits=6}

Numerical_Methods::Course
Numerical_Methods = {name="Numerical_Methods", credits=4}

Compilers::Course
Compilers = {name="Compilers", credits=4}

treea = (Node Functional (Node Astronomy (Node Programming (Node Astronomy Leaf Leaf) (Node Thermo_Dynamics Leaf Leaf)) Leaf) (Node Basic (Node Compilers (Node Relativity Leaf Leaf) (Node Astronomy (Node Analysis Leaf Leaf) Leaf)) Leaf))

treeb = (Node Functional (Node Astronomy (Node Programming (Node Astronomy Leaf Leaf) Leaf) Leaf) (Node Relativity (Node Compilers (Node Numerical_Methods Leaf Leaf) (Node Astronomy (Node Analysis Leaf Leaf) Leaf)) Leaf))

treec = (Node Analysis Leaf (Node Programming Leaf (Node Astronomy Leaf (Node Basic Leaf (Node Compilers Leaf (Node Thermo_Dynamics Leaf (Node Numerical_Methods Leaf (Node Functional Leaf Leaf))))))))


:: Tree a = Node a (Tree a) (Tree a) 
          | Leaf


calc :: (Tree Course) -> Int
calc Leaf = 0
calc (Node n le ri) = (n.credits) + (calc le) + (calc ri)

replace :: (Tree Course) -> (Tree Bool)
replace (Node n Leaf Leaf) = (Node False Leaf Leaf)
replace (Node n Leaf ri) = (Node True Leaf (replace ri))
replace (Node n le Leaf) = (Node False (replace le) Leaf)
replace (Node n le ri)
| (calc le) < (calc ri) = (Node True (replace le) (replace ri))
=  (Node False (replace le) (replace ri)) 


averages :: (Tree Course) -> (Tree Bool)
averages tree = replace tree 



//Start = averages treea
//(Node True (Node False (Node False (Node False Leaf Leaf) (Node False Leaf Leaf)) Leaf) (Node False (Node True (Node False Leaf Leaf) (Node False (Node False Leaf Leaf) Leaf)) Leaf))
//Start = averages treeb
//(Node True (Node False (Node False (Node False Leaf Leaf) Leaf) Leaf) (Node False (Node True (Node False Leaf Leaf) (Node False (Node False Leaf Leaf) Leaf)) Leaf))
//Start = averages treec
//(Node True Leaf (Node True Leaf (Node True Leaf (Node True Leaf (Node True Leaf (Node True Leaf (Node True Leaf (Node False Leaf Leaf))))))))


/*
--------------------------------------TASK 2 - 40 POINTS--------------------------------------------------------

Given the root of a binary tree, return the sum of values of nodes with an even-valued grandparent. 
If there are no nodes with an even-valued grandparent, return 0.
A grandparent of a node is the parent of its parent if it exists.

*/

isLeaf :: (Tree a) -> Bool
isLeaf Leaf = True
isLeaf _ = False



getChildren :: (Tree a) -> [a] 
getChildren (Node n l r)
| isLeaf l && isLeaf r =[]
| isLeaf l = [extractN r]
| isLeaf r = [extractN l]
= [extractN l] ++ [extractN r] 

  
sumEvenGrandparent :: (Tree Int) -> Int
sumEvenGrandparent (Node n le ri)
| isEven n = sum ((getChildren le) ++ (getChildren ri))
= 0

Tree1 :: Tree Int
Tree1 = Node 12 (Node 1 (Node 3 Leaf Leaf) (Node 4 Leaf Leaf))  (Node 2 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf))
//root is 12 --> even, it has 4 grandchildren: 3,4,5,6. Their sum is 18.

Tree2 :: Tree Int
Tree2 = Node 24 (Node 1 (Node 3 Leaf (Node 8 Leaf Leaf)) Leaf)  (Node 2 Leaf Leaf)

//Start = sumEvenGrandparent Tree1//18
//Start = sumEvenGrandparent Tree2//3 --> root is even but it has just one grandchild 3


/*
--------------------------------------TASK 3 - 30 POINTS--------------------------------------------------------

Given a tree and an integer. Return the count of node in that level.
The level where root exists is counted as level 1.

*/

tree1 = Node 2 (Node 5 (Node 3 (Node 24 Leaf Leaf) Leaf) (Node 6 Leaf (Node 7 Leaf Leaf))) (Node 8 (Node 15 Leaf (Node 10 Leaf (Node 14 Leaf Leaf))) (Node 13 Leaf (Node 16 Leaf Leaf)))
tree2 = Node "abc" (Node "ghi" (Node "func" (Node "zyh" Leaf Leaf) Leaf) (Node "tional" Leaf (Node "haha" Leaf Leaf))) (Node "xyz" (Node "program" Leaf (Node "zyx" Leaf Leaf)) (Node "ming" Leaf (Node "cba" Leaf Leaf)))
 
extractN :: (Tree a) -> a
extractN (Node n le ri) = n

depth :: (Tree a) -> Int
depth Leaf = 0
depth (Node n le ri) = (max (depth le) (depth ri)) + 1   

helper :: (Tree a) Int Int -> Int
helper Leaf d cd = 0
helper (Node n le ri) d cd
| cd == d = 1
= helper le d (cd+1) + helper ri d (cd+1)
 

nodeCountLevel :: (Tree a) Int -> Int
nodeCountLevel Leaf t = 0
nodeCountLevel tree t = helper tree t 1
 
 
//Start = nodeCountLevel tree1 5  // 1
//Start = nodeCountLevel tree2 6  // 0
//Start = nodeCountLevel tree1 3  // 4
//Start = nodeCountLevel tree1 1  // 1
//Start = nodeCountLevel tree2 2  // 2

