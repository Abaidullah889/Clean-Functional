module HW8
import StdEnv

//Write your name and neptun code here: <    Abaidullah Asif  , ZLWD2B    >

//There's no chance not to notice plagiarism here :)

//COME ON GUYS, WE'RE ALMOST THERE!!! It's worth trying for several hours!

//Trust me it took more time for me to prepare this, than for you to solve ;)

/*
--------------------------------------------TASK 1 - 25 POINTS: --------------------------------------------

Define a type of hotel room (TypeOfRoom - Algebraic Datatype) which can be one of this four:
Penthouse, Deluxe or Standard.
Define type Room that has three fields: 
available (Boolean) - showing if the room is available or not,
type (TypeOfRoom) - showing if it is Penthouse, Deluxe or Standard,
price (int) - showing price of the room.
Define type Guest, which has three fields: 
name - string
money - int
desiredRoom (TypeOfRoom) - showing in which room does she/he wanna stay.
Your input is a Guest and an array of Rooms, your output should be String.
If the room is available and person has enough money to stay there print:
"It's yours, now go to your room and code CLEAN"
If not: 
"You can't always get what you want ;)"

*/

:: Guest = { name :: String , 
		     money :: Int ,
		     desiredRoom :: TypeOfRoom
		   }
		     
 
:: TypeOfRoom = Penthouse | Deluxe | Standard

:: Room = { 
			available :: Bool ,
			type :: TypeOfRoom , 
			price :: Int 
		  }
 
//This function is for you to be able to compare two types of rooms, nevermind, I will teach you how it works
//in the last week 

instance == TypeOfRoom 
where
    (==) Penthouse Penthouse = True
    (==) Deluxe Deluxe = True
    (==) Standard Standard = True
    (==) _ _ = False

room1 :: Room
room1 = {available = True, type = Penthouse, price = 1500}

room2 :: Room
room2 = {available = False, type = Penthouse, price = 2000}

room3 :: Room
room3 = {available = False, type = Deluxe, price = 870}

room4 :: Room
room4 = {available = True, type = Deluxe, price = 950}

room5 :: Room
room5 = {available = True, type = Standard, price = 500}

guest1 :: Guest
guest1 = {name = "Anna", money = 3000, desiredRoom = Penthouse}

guest2 :: Guest
guest2 = {name = "George", money = 700, desiredRoom = Deluxe}

guest3 :: Guest
guest3 = {name = "Aman", money = 1500, desiredRoom = Standard}

toList :: {a} -> [a]
toList arr = [el \\ el<-: arr]

assistant :: Guest Room -> Bool
assistant g r
| r.available == True && g.desiredRoom == r.type && g.money >= r.price = True
= False 

computer :: Guest [Room] -> String
computer g [] = "You can't always get what you want ;)"
computer g [x:xs]
| assistant g x = "It's yours, now go to your room and code CLEAN"
= computer g xs


check :: Guest {Room} -> String
check g arr = computer g (toList arr)




//Start = check guest1 {room1, room2, room3, room4, room5}//"It's yours, now go to your room and code CLEAN"
//Start = check guest2 {room1, room2, room3, room4, room5}//"You can't always get what you want ;)"
//Start = check guest3 {room1, room2, room3, room4, room5}//"It's yours, now go to your room and code CLEAN"




/*
--------------------------------------------TASK 2 - 75 POINTS: --------------------------------------------

You are given an array of Oceans. Each ocean has 2 fields: name and array of seas. Sea itself has three fields: 
name, matrix and economic importance. We don't know economic importance of any given sea in the beginning, your
role is to find out and update the field EconomicImportance for every sea in every ocean.
Economic importance can be either High, Average or Low. Sea has high economic importance if its number of 
islands are more than 4, if it has just one island or none it has low economic importance, otherwise 
its economic importance is average.

Here is how you should calculate number of islands in the sea using its field - matrix:

Given an M x N 2D matrix which represents a map of lands and water, count the number of islands.
An island is character that is surrounded by water from every side. 
You may assume all four edges of the matrix are all surrounded by water.
Island is represented by any single character (they are not repeated), water is represented with '0'.

Example:

matrix = [
 ['$', '0', '0', '0'],
 ['0', '0', '#', '0'],
 ['0', 'A', '0', '0'],
 ['0', 'p', '0', '0']
]  --> the number of islands is 2

So in this matrix we see only 2 islands '$' and '#', because only they are surrounded with water from all
4 sides, 'A' and 'p' are not islands cause they surround each other on one side.

*/



:: EconomicImportance = High | Average | Low | ?

:: Sea = {name :: String,
	matrix :: [[Char]],
	economic_importance :: EconomicImportance
 }

:: Ocean = {name :: String,
	seas :: {Sea}
 }
 
sea1 :: Sea
sea1 = {name = "Bering Sea", matrix = matrix1, economic_importance = ?}

sea2 :: Sea
sea2 = {name = "Sea of Japan", matrix = matrix2, economic_importance = ?}

sea3 :: Sea
sea3 = {name = "Coral Sea", matrix = matrix3, economic_importance = ?}

sea4 :: Sea
sea4 = {name = "North Sea", matrix = matrix4, economic_importance = ?}

sea5 :: Sea
sea5 = {name = "Caribbean Sea", matrix = matrix5, economic_importance = ?}

sea6 :: Sea
sea6 = {name = "Barents Sea", matrix = matrix6, economic_importance = ?}

sea7 :: Sea
sea7 = {name = "Chukchi Sea", matrix = matrix7, economic_importance = ?}

ocean1 :: Ocean
ocean1 = {name="Pacific Ocean", seas={sea1, sea2, sea3}}

ocean2 :: Ocean	
ocean2 = {name="Atlantic Ocean", seas={sea4, sea5}}

ocean3 :: Ocean	
ocean3 = {name="Arctic Ocean", seas={sea6, sea7}}

matrix1 = [['A', '0', 'B', '0', '0', 'C', '0', 'D', 'E', '0', 'F', '0', 'G'],
    ['H', 'I', 'J', '0', 'K', '0', 'L', '0', 'M', '0', 'N', 'O', 'P'],
    ['0', 'Q', 'R', 'S', '0', '0', '0', 'T', '0', '0', 'U', 'V', '0'],
    ['W', '0', 'X', '0', 'Y', 'Z', 'A', '0', 'B', '0', 'C', '0', 'D'],
    ['A', '0', 'A', 'A', 'A', '0', 'I', '0', 'J', 'K', '0', 'L', 'M'],
    ['A', 'A', '0', 'A', 'A', '0', 'R', 'S', '0', 'T', 'U', '0', 'V'],
    ['A', '0', '0', 'A', '0', '0', 'Y', 'Z', 'B', 'B', '0', '0', 'C'],
    ['0', 'B', '0', '0', 'B', '0', 'F', '0', 'G', 'H', 'I', '0', '0'],
    ['B', 'B', '0', '0', 'B', '0', 'M', 'N', '0', '0', 'O', 'P', '0'],
    ['0', 'B', 'B', '0', '0', 'S', '0', 'B', '0', '0', 'U', 'V', '0']]  //--> 9 islands <==> High

matrix2 = [['$', '0', '0', '0'],
           ['0', '0', '#', '0'],
           ['0', 'A', '0', '0'],
           ['0', 'p', '0', '0']] // --> 2 islands <==> Average

matrix3 = [['0', '0', '0'],
           ['0', '0', '0'],
           ['0', '0', '0']] // --> 0 islands <==> Low
 
matrix4 = [['A']] // --> 1 islands <==> Low

matrix5 = [['A', '0', 'B', '0'],
           ['0', '0', '0', '0'],
           ['C', '0', 'D', '0'],
           ['0', '&', '0', '%']] // --> 6 islands <==> High
    
matrix6 = [['e', '0', 'f'],
	       ['0', 'a', '0'],
	       ['0', '0', '0'],
	       ['0', 'b', '0'],
	       ['0', '0', '0'],
	       ['0', 'c', '0'],
	       ['0', '0', '0'],
	       ['0', 'd', '0'],
	       ['0', '0', '0'],
	       ['g', '0', 'h']] // --> 8 islands <==> High
	
matrix7 = [['0', '{', '0', '}'],
	       ['0', '0', ')', '0'],
	       [')', '0', '0', '0'],
	       ['0', '0', '*', '*']] // --> 4 islands <==> Average



//////////////////////////////////////////////////////////////
matrixRows :: [[Char]] -> Int
matrixRows matrix = length matrix

matrixCols :: [[Char]] -> Int
matrixCols [x:xs] = length x

////////////////////////////////////////////////////////

leftHelper :: Char [Char] Int -> Bool
leftHelper c arr cpos
| arr !! (cpos-1) == '0' = True
= False  

leftCheck :: Char [[Char]] Int Int -> Bool
leftCheck c matrix row col 
| col == 0 = True
| leftHelper c (matrix !! row) col = True
= False


rightHelper :: Char [Char] Int -> Bool
rightHelper c arr cpos
| arr !! (cpos+1) == '0' = True
= False


rightCheck :: Char [[Char]] Int Int Int -> Bool
rightCheck c matrix row col mcol
| (col + 1) == mcol = True
= rightHelper c (matrix !! row) col 


topHelper :: Char [Char] Int -> Bool
topHelper c arr col
| arr !! col == '0' = True
= False

topCheck :: Char [[Char]] Int Int -> Bool
topCheck c matrix row col 
| row == 0 = True
= topHelper c (matrix !! (row-1)) col


bottomHelper:: Char [Char] Int -> Bool
bottomHelper c arr col
| arr !! col == '0' = True
= False

bottomCheck:: Char [[Char]] Int Int Int Int -> Bool
bottomCheck c matrix row col mrow mcol
| (row+1) == mrow = True
= bottomHelper c (matrix !! (row+1)) col
 

verify:: Char [[Char]] Int Int -> Bool
verify c matrix row col 
| (leftCheck c matrix row col) && (rightCheck c matrix row col (matrixCols matrix)) && (topCheck c matrix row col) && (bottomCheck c matrix row col (matrixRows matrix) (matrixCols matrix)) = True
= False 


rowCheck :: [Char] [[Char]] Int Int -> Int
rowCheck [] _ _ _ = 0
rowCheck [x:xs] matrix str stc
| x <> '0' && (verify x matrix str stc) = 1 + rowCheck xs matrix str (stc+1) 
= rowCheck xs matrix str (stc+1)  


Islands :: [[Char]] [[Char]] Int Int -> Int
Islands [] _ _ _ = 0
Islands [x:xs] matrix str stc = (rowCheck x matrix str stc) + Islands xs matrix (str+1) stc 



updateVal :: Sea Int -> Sea
updateVal s n
| n > 4 = {s & economic_importance = High}
| n > 1 && n <= 4 = {s & economic_importance = Average}
= {s & economic_importance = Low}

updateSeas :: {Sea} -> {Sea}
updateSeas arr = {updateVal el (Islands (el.matrix) (el.matrix) 0 0) \\ el <-: arr}

updateOcean :: Ocean -> Ocean
updateOcean oce = {oce & seas = updateSeas oce.seas} 

updateEconomicImportance :: {Ocean} -> {Ocean}
updateEconomicImportance arr = {updateOcean el \\ el <-: arr}


Start = updateEconomicImportance {ocean1, ocean2, ocean3} 


/* ------------------------- ANSWER : -----------------------------

{(Ocean "Pacific Ocean" 
{(Sea "Bering Sea" [
['A','0','B','0','0','C','0','D','E','0','F','0','G'],
['H','I','J','0','K','0','L','0','M','0','N','O','P'],
['0','Q','R','S','0','0','0','T','0','0','U','V','0'],
['W','0','X','0','Y','Z','A','0','B','0','C','0','D'],
['A','0','A','A','A','0','I','0','J','K','0','L','M'],
['A','A','0','A','A','0','R','S','0','T','U','0','V'],
['A','0','0','A','0','0','Y','Z','B','B','0','0','C'],
['0','B','0','0','B','0','F','0','G','H','I','0','0'],
['B','B','0','0','B','0','M','N','0','0','O','P','0'],
['0','B','B','0','0','S','0','B','0','0','U','V','0']] High),    
(Sea "Sea of Japan" [
['$','0','0','0'],
['0','0','#','0'],
['0','A','0','0'],
['0','p','0','0']] Average),
(Sea "Coral Sea" [
['0','0','0'],
['0','0','0'],
['0','0','0']] Low)}),
(Ocean "Atlantic Ocean" 
{(Sea "North Sea" [['A']] Low),
(Sea "Caribbean Sea" [
['A','0','B','0'],
['0','0','0','0'],
['C','0','D','0'],
['0','&','0','%']] High)}),
(Ocean "Arctic Ocean" 
{(Sea "Barents Sea" [
['e','0','f'],
['0','a','0'],
['0','0','0'],
['0','b','0'],
['0','0','0'],
['0','c','0'],
['0','0','0'],
['0','d','0'],
['0','0','0'],
['g','0','h']] High),
(Sea "Chukchi Sea" [
['0','{','0','}'],
['0','0',')','0'],
[')','0','0','0'],
['0','0','*','*']] Average)})}
*/