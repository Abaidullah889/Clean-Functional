module example
import StdEnv

//-------------------------------------------------------------------------------
/*
	1. Create a `toInt` instance for the Person record. An integer representation of a person
	is the sum of the length of its firstName, their age and height.
*/

::Person={firstName::String, age::Int, height::Int}
Rose::Person
Rose={firstName="Rose",age=23,height=172}
Jack::Person
Jack={firstName="Jack",age=25,height=193}
Emilia::Person
Emilia={firstName="Emilia",age=15,height=160}
Leo::Person
Leo={firstName="Leo",age=16,height=175}
Grace::Person
Grace={firstName="Grace",age=35,height=165}
Harry::Person
Harry={firstName="Harry",age=42,height=180}
Emilia2::Person
Emilia2={firstName="Emilia",age=15,height=180}

// TO DO instance
Slength :: String -> Int
Slength arr = length [x \\ x <-: arr]

instance toInt Person
	where
	toInt p = p.age + p.height + Slength p.firstName
		

//Start = toInt Rose // 199
//Start = toInt Leo // 194
//Start = toInt Grace // 205


//-------------------------------------------------------------------------------
/*
	2. Create an instance of `isEven` for the Person record. A person is even if the sum of their
	age and height is even.
*/
// TO DO instance
instance isEven Person 
	where
		isEven p = isEven (p.age + p.height)

//Start = isEven Rose // False
//Start = isEven Harry // True


//-------------------------------------------------------------------------------
// 8.
// Write a filter function for colored rose tree.
// Colored rose Tree is a tree where each node has 
// some value, color and children nodes stored in list.
// Your filter function should take tree, color, a two 
// condition function and filtering type as an argument. Return a list of
// values stored in nodes which have given color and
// satisfy both of the given conditions if filter type is 'AND'
// or satisfy at least one of the given functions if filter type
// is "OR" (Condition function returns
// true for node's value).

:: NodeColor = Red | Green | Blue

:: FilterType = AND | OR

:: ColoredRoseTree a = Node a NodeColor [ColoredRoseTree a] | Leaf


// TODO
instance == FilterType
	where
		(==) OR OR = True
		(==) AND AND = True
		(==) _ _ = False
		
		
instance == NodeColor
	where
		(==) Red Red = True
		(==) Green Green = True
		(==) Blue Blue = True
		(==) _ _ = False

filterColoredTree :: (ColoredRoseTree a) NodeColor FilterType (a -> Bool) (a -> Bool) -> [a]
filterColoredTree Leaf _ _ _ _ = []
filterColoredTree (Node x c children) color type f1 f2
| type == OR && ((f1 x) || (f2 x)) && (color == c) = [x] ++ flatten [filterColoredTree child color type f1 f2 \\ child <- children]
| type == AND && ((f1 x) && (f2 x)) && (color == c) = [x] ++ flatten [filterColoredTree child color type f1 f2 \\ child <- children]
= flatten [filterColoredTree child color type f1 f2 \\ child <- children]
		
tree1 = Node 1 Red [(Node 2 Blue [Node 4 Blue []]), Leaf, Leaf, (Node 3 Blue [Leaf,Leaf])]
tree2 = Node 1 Red [(Node 2 Blue [Node 4 Blue []]), Leaf, Leaf, (Node 3 Blue [Leaf,Node 7 Red [Node 9 Red [], Node 10 Red []]])]

//Start = filterColoredTree tree1 Blue OR isEven isOdd // [2,4,3]
//Start = filterColoredTree tree1 Blue AND isEven isOdd // []
//Start = filterColoredTree tree1 Blue AND isOdd isOdd // [3]
//Start = filterColoredTree tree2 Red OR (\x = True) isEven // [1,7,9,10]
//Start::[Int] // Uncomment this line too, to run next test
//Start = filterColoredTree Leaf Green OR isOdd isEven // []


:: Beer = {name :: String, price :: Real, ratings :: [Int]}
// instances originally were not given

instance == Beer 
  where 
     (==) b1 b2 = b1.name == b2.name && b1.price == b2.price
instance < Beer 
  where 
     (<) b1 b2 = (toReal (sum b1.ratings)/toReal (length b1.ratings)) < (toReal (sum b2.ratings)/toReal (length b2.ratings))
:: Beer = {name :: String, price :: Real, ratings :: [Int]}
// instances originally were not given

instance == Beer 
  where 
     (==) b1 b2 = b1.name == b2.name && b1.price == b2.price
instance < Beer 
  where 
     (<) b1 b2 = (toReal (sum b1.ratings)/toReal (length b1.ratings)) < (toReal (sum b2.ratings)/toReal (length b2.ratings))

/** * 10. Write a function that takes a list of records of places and their coordinates 
and print the names of the 2 places where the taxicab distance between two of them is minimum. 
The taxicab distance is the sum of the absolute differences of their Cartesian coordinates) |x1 - x2| + |y1 - y2| */

//:: Place = {x :: Int, y :: Int, name1 :: String}

instance == Place 
  where 
     (==) b1 b2 = b1.name1 == b2.name1 && b1.x == b2.x && b1.y == b2.y

dist :: Place Place -> Real
dist a b = sqrt (toReal((b.x-a.x)^2+(b.y-a.y)^2))

NearestPair :: [Place] -> (String, String)
NearestPair list = hd [(a.name1,b.name1) \\  a<-list, b<-list | dmin == dist a b  ]
   where dmin = minList [dist x y \\ x<-list, y<-list | (not (x==y))]

//Start = NearestPair [Deak, Nyugati, ELTE,  Corvin] //("Nyugati Palyaudvar","Corvin Negyed")
//Start = NearestPair [KoKi, Keleti] //(“Kobanya Kispest”, “Keleti Palyaudvar”)
//Start = NearestPair [Nyugati, Keleti, ELTE, Deak, KoKi] //(“Nyugati Palyaudvar”, “Keleti Palyaudvar”)

//12. Create an instance of ‘==’ by checking equality of trees.

:: TreeL a = LeafL a
		   | NodeL (TreeL a) (TreeL a)

instance == (TreeL Int)
where
    (==) (LeafL x1) (LeafL x2) =  (x1==x2)
    (==) (NodeL l1 r1) (NodeL l2 r2) = and [l1==l2, r1==r2]
    (==) _ _ = False


//Start = (LeafL 2) == (NodeL (LeafL 2) (LeafL 3)) // False
//Start = (LeafL 2) == (LeafL 2) // True
//Start = (NodeL (LeafL 2) (LeafL 3)) == (NodeL (LeafL 2) (LeafL 3))













