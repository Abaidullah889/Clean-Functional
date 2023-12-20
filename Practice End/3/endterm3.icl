module endterm3
import StdEnv

/*---------------------------------------------------------------
-- Functional Programming end-term

-- This solution was submitted and prepared by
-- <name, neptun> for the end-term programming assignment of
-- the Functional Programming course.

-- I declare that this solution is my own work.

-- I have not copied or used third-party solutions.

-- I have not passed my solution to my classmates, neither made it public.

-- Students' regulation of Eotvos Lorand University (ELTE Regulations Vol. II. 74/C.)
-- states that as long as a student presents another student’s work -
-- or at least the significant part of it - as his/her own performance,
-- it will count as a disciplinary fault.

-- The most serious consequence of a disciplinary fault can be dismissal
-- of the student from the University.
*/

 

//---------------

/* 1. Arrays. 10 points
Given an array of integers, remove the elements that have even occurances in the array.
*/
count :: Int [Int] -> Int
count n [] = 0
count n [x:xs] 
|n ==x = 1 + count n xs
= count n xs
removeOcc :: {Int} -> {Int}
removeOcc numbers = {x \\x <-: numbers |isOdd(count x [y \\y <-: numbers])}
//Start = removeOcc {1,1,2,2,2,3,3,4,5,6,6,6,6} //{2,2,2,4,5}
//Start = removeOcc {1,1} // {}
//Start = removeOcc {1,1,1} // {1,1,1}
//Start = removeOcc {1,1,2,2,3,4,5} // {3,5,6}

//---------------

/* 2. Generator. 10 points
Given a positive integer value n, generate an array that for n=10
has as elements 1,2,2,3,3,3,4,4,4,4,...,10,...,10.
*/

generate :: Int -> {Int}
generate n = {ar\\ar<-(flatten[repeatn x x\\ x <- [1..n]])}
//Start :: {Int} // this is needed
//Start = generate 10 // {1,2,2,3,3,3,4,4,4,4,...,10,...,10}
//Start = generate 4 // {1,2,2,3,3,3,4,4,4,4}

//---------------

:: City = BUDAPEST | GYOR | DEBRECEN
:: Product = {productName :: String , price :: Real}
:: Shop = {shopName :: String , products :: {Product}, location :: City}

meat = {productName ="meat" ,price= 5000.123}
fruits={productName ="fruits" , price=2000.123}
vegetables = {productName ="vegetables",price=1700.50}

aldi = {shopName = "aldi" , products = {meat,fruits,vegetables} ,location = BUDAPEST}
spar = {shopName = "spar" , products = {{productName ="meat" ,price= 4500.0},fruits,vegetables} , location = GYOR}
lidl = {shopName = "lidl" , products = {meat,fruits,vegetables} ,location = BUDAPEST}
abc = {shopName = "abc" , products = {{productName ="meat" ,price= 4500.0},{productName ="fruits" ,price= 1700.0},{productName ="vegetables" ,price= 1500.0}} , location = GYOR}

//---------------

/* 3. Shops. 10 points
Given an array of shops, all the shopes have the same products but with different
price, return a tuple containing the name of the cheapest shop and it's location.
the cheapest shop is the shop in the array whose sum of its products prices
is the smallest in the array.
Note : if there is more than one cheap shop in the array, return the first one.
Assume that the given array is not empty.
*/

instance < Shop
	where
	(<) a b = avg[x.price \\x <-: a.products] < avg[x.price \\x <-: b.products] 
cheapestShop :: {Shop} -> (String,City)
cheapestShop shops = (shop.shopName, shop.location)
	where shop = hd(sort[x\\x<-:shops])
//Start = cheapestShop {aldi,spar,lidl,abc} // ("abc",GYOR)
//Start = cheapestShop {aldi,spar} // ("spar",GYOR)
//Start = cheapestShop {lidl,aldi} // ("lidl",BUDAPEST)

//---------------

:: Person = {name :: String, age :: Int , numbers :: {Int}}
abdullah :: Person
abdullah = {name = "Abdullah", age = 13 , numbers = {-4,3,2,1} }
abood :: Person
abood = {name = "abood", age = 12 , numbers = {3,-6,5,-2,1} }
othman :: Person
othman = {name = "othman", age = 12 , numbers = {-5,4,-2,3,1} }
mohammed :: Person
mohammed = {name = "Mohammed" , age = 18 , numbers = {-5,4,-2,3,1,-1,-6,-1,0,5}}

//---------------

/* 4. Vectors. 30 points BE AWARE THAT THIS TASK CONSISTS OF MULTIPLE SMALL TASKS.
A vector in programming is a dynamic implementation of the Array data structure
Create the pushBack, pushFront, remove, indexOf, and swap operations for this type.
*/
//Start = map inc (takeWhile ((<>)0) [1,2,3,4,5,0,7,86,37357,37373])
//Start = unzip [(1,1), (2,2), (3,3)]
/*
:: Point = {  x       ::  Real
            , y       ::  Real
            , visible ::  Bool
            }

:: Vector = { dx       ::  Real
            , dy       ::  Real
            }
  
Origo :: Point
Origo = { x = 0.0
        , y = 0.0
        , visible = True
        }
Dist :: Vector
Dist = { dx = 1.0
       , dy = 2.0
       }

IsVisible :: Point -> Bool
IsVisible {visible = True} = True //de ce paranteze rotunde???///////////////////////////////////
IsVisible _                = False
*/
//Start = IsVisible Origo
:: Vector a :== [a]

/* 4.1 5 points
pushBack is a function that takes a vector and an element and
adds the element to the end of the vector
*/

pushBack :: (Vector a) a -> (Vector a)
pushBack list n = list ++[n]

//Start = pushBack [1,2,3] 4 //[1,2,3,4]
// Start = pushBack [1,0,213] 10000 //[1,0,213,10000]

/* 4.2 5 points
pushFront is a function that takes a vector and an element and
adds the element to the beginning of the vector
*/

pushFront :: (Vector a) a -> (Vector a)
pushFront list a = [a] ++ list
//Start = pushFront [1,0,213] 10000 //[10000,1,0,213]
// Start = pushFront [1,2,3] 4 //[4,1,2,3]

/* 4.3 5 points
remove is a function that takes a vector and an element and
removes the element from the vector
If it exists, and returns it. Otherwise it returns an error.
*/

remove :: (Vector a) a -> (Vector a)|Eq a
remove [] n = abort "Element does not exist"
remove list n 
|hd list == n = tl list
= [hd list] ++ remove (tl list) n

//Start = remove [1,2,3] 2 //[1,3]
// Start = remove [1,0,213] 10000 //"Element does not exist"

/* 4.4 5 points
indexOf is a function that takes a vector and an element
and returns the element's index in the vector (counting from 0)
If it exists otherwise it returns an error.
*/

indexOf :: (Vector a) a -> Int | Eq a
indexOf vector n
|list == [] = abort "Element does not exist"
= hd list
	where list = [i \\ i <- [0.. ((length vector)-1)]| vector!!i==n]

//Start = indexOf [1,2,3] 2 // 1
// Start = indexOf [1,0,213] 10000 //"Element does not exist"

/* 4.4 10 points
swap is a function that takes a vector and two elements and swaps the two elements in the vector
if they both exist, otherwise it returns an error
*/
// removeAt
/*
swap :: (Vector a) a a -> (Vector a) | Eq a
swap [] n1 n2 = abort "Element"
swap vector n1 n2 = updateAt (fst(removeIndex n2 firstupdate)) n1///////////////////////////////////////////////////////////////////
where 
	 firstupdate = updateAt (fst(removeIndex n1 vector)) n2
*/

swap :: (Vector a) a a -> (Vector a) | Eq a
swap vector a b
| (isMember a vector) && (isMember b vector) && (upd <> vector) = upd
| (isMember a vector) && (isMember b vector) = swap vector b a
= abort "Element does not exist"
where upd = updateL b a (updateL a b vector)

updateL :: a a [a] -> [a] | Eq a
updateL n up list = (takeWhile ((<>)n) list) ++ [up] ++ (tl (dropWhile ((<>)n) list))
//Start = swap [1,2,4,5,6,3,888,9,7] 1 3 // [3,2,4,5,6,1,888,9,7]
// Start = swap [1,0,213] 10000 0 //"Element does not exist"

//---------------

/* 5. Triples. 10 points
For a given n generate a list of triple pairs with numbers for 1 to n,
their cubes and triples.
*/

triples :: Int -> [(Int,Int,Int)]
triples n = [(i,i*i*i, i*3)\\i <- [1..n]]
//Start = triples 2 // [(1,1,3),(2,8,6)]
//Start = triples 4 // [(1,1,3),(2,8,6),(3,27,9),(4,64,12)]

//---------------

/* 6. Merge class. 10 points
Create a class Merge which has operations sorted, mess and has the neutral element Empty.
The sorted and mess are doing the following operations:
sorted -> merges sorted lists and returns sorted list. If a list is not sorted, replace it with
empty list and merge.

mess -> merges lists from the beginning of the first one and follows from the last of the second.

for instance: mess [1,2,3,5] [9,8,10] = [1,10,2,8,3,9,5]

Empty -> empty list

After that create an instance for [Int].

*/

class Merge a 
where
	Empty :: [a]
	mess :: [a] [a] -> [a]
	sorted :: [a] [a] -> [a]

instance Merge Int
where
	Empty :: [Int]
	Empty = []
	
	sorted :: [Int] [Int] -> [Int]
	sorted x y 
	|x <> sort x = y
	|y <> sort y = x
	= sort(x++y)
	
	mess :: [Int] [Int] -> [Int]
	mess [] bs = reverse bs
	mess as [] = as 
	mess as bs = [hd as, last bs] ++ (mess (tl as) (init bs))
	

//Start = mess [1,2,3,5] [9,8,10] // [1,10,2,8,3,9,5]
//Start = sorted [1..10] [7..15] // [1,2,3,4,5,6,7,7,8,8,9,9,10,10,11,12,13,14,15]
//Start = sorted [3..7] Empty // [3,4,5,6,7]
//Start = sorted Empty [1,3,7,4,2] // []
//Start = mess Empty [1..10] // [10,9,8,7,6,5,4,3,2,1]

//---------------

/* 7. BST. 10 points
Write a binary search tree type. Build from an arbitrary list a binary search tree
then collect from the tree the elements (which by this would be sorted)
*/
:: Tree a = Node a (Tree a)(Tree a)
     	  |Leaf

bsearch :: [Int] -> Tree Int
bsearch [] = Leaf
bsearch [x:xs] = insert x(bsearch xs)

insert :: Int (Tree Int) -> Tree Int
insert e Leaf = (Node e Leaf Leaf)
insert e (Node x le ri)
|e <= x = (Node x (insert e le) ri)
|e > x = (Node x le (insert e ri))


//Start = bsearch [5,6,7,3,2,5,7,9,60,63,12,45,7,4,3]
bcollect :: (Tree Int) -> [Int]
bcollect Leaf = []
bcollect (Node x le ri) = (bcollect le) ++ [x] ++ (bcollect ri)

mytree = (Node 3 (Node 2 Leaf (Node 3 Leaf Leaf)) (Node 4 Leaf (Node 7 (Node 7 (Node 5 (Node 5 Leaf Leaf) (Node 7 (Node 6 Leaf Leaf) Leaf)) Leaf) (Node 45 (Node 12 (Node 9 Leaf Leaf) Leaf) (Node 63 (Node 60 Leaf Leaf) Leaf)))))

//Start = bcollect mytree


//---------------

/* 8. FlexTree. 10 points
Flex Tree can have 4 types of nodes: Ternary, Binary, Unary and Terminal. As
names suggest these nodes have 3, 2, 1 and 0 children nodes repsectively.
Terminal nodes do not store any value, they indicate end of the tree.

Write a function that takes a FlexTree as an argument and converts
it to the list with following rules:
* TerminalNode should be converted to empty list.
* UnaryNode's child subtree should be converted to the list and this node's
value should be appended from front.
* BinaryNode's left and right children subtrees should be converted in order
and this node's value should be inserted after the left subtree values and
before the right subtree values.
* TernaryNode's left, mid and right children subtrees should be converted in
order and this node's value should be inserted after the left subtree values
and before the mid subtree values.

For example, if we have a FlexTree:
(TernaryNode 1)
/ | \
(BinaryNode 2) TerminalNode (UnaryNode 3)
/ \ |
TerminalNode (UnaryNode 4) (BinaryNode 5)
| / \
TerminalNode TerminalNode TerminalNode

After converting it to the list with these rules we get:
[2, 4, 1, 3, 5]
*/

 

:: FlexTree a = TernaryNode a (FlexTree a) (FlexTree a) (FlexTree a)
                            | BinaryNode a (FlexTree a) (FlexTree a)
                            | UnaryNode a (FlexTree a)
                            | TerminalNode

ftree1 = UnaryNode 1 (BinaryNode 2 TerminalNode TerminalNode)
ftree2 = BinaryNode 1 TerminalNode ftree1
ftree3 = TernaryNode 3 TerminalNode (UnaryNode 3 TerminalNode) (UnaryNode 3 TerminalNode)
ftree4 = TernaryNode 1 ftree2 TerminalNode (BinaryNode 2 (TernaryNode 1 TerminalNode TerminalNode TerminalNode) (BinaryNode 2 ftree2 ftree3))

 

flexTreeToList :: (FlexTree a) -> [a]
flexTreeToList TerminalNode = []
flexTreeToList (TernaryNode x le mid ri) = flexTreeToList le ++ [x] ++ flexTreeToList mid ++ flexTreeToList ri
flexTreeToList (BinaryNode x le ri)=flexTreeToList le ++ [x] ++ flexTreeToList ri
flexTreeToList (UnaryNode x last)= [x] ++ flexTreeToList last
// Start = flexTreeToList TerminalNode // []
//Start = flexTreeToList ftree1 // [1,2]
//Start = flexTreeToList ftree2 // [1,1,2]
// Start = flexTreeToList ftree3 // [3,3,3]
// Start = flexTreeToList ftree4 // [1,1,2,1,1,2,1,1,2,2,3,3,3]

//---------------

