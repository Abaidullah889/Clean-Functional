module HW7
import StdEnv

//Write your name and nep code here: <Abaidullah Asif, ZLWD2B>

//Remember plagiarism is zero for both! (And from now on the hw-s will be hard, so it'll be easy to notice copying :) 

//Comment: Using arrays is not mandatory.

//----------------------1-------------------------
/*
You are given a text. You should make sure that in that text every new sentence starts with capital letter!
Example:
Input:
"Hi. i need your advice! currently I am FAILING every subject, so do you think I should change my major? i am so screwed."
Output: 
"Hi. I need your advice! Currently I am FAILING every subject, so do you think I should change my major? I am so screwed."
*/


toList :: String -> [Char]
toList str = [a \\ a<-: str]



helper :: Char Int [Char] -> Char
helper a n lst
| n == 0 = toUpper a
| lst!!(n-1) == ' ' && (lst!!(n-2)=='.' || lst!!(n-2)=='?' || lst!!(n-2)=='!' ) = toUpper a  
= a


everySentenceWithUpper :: String -> String
everySentenceWithUpper str = {(helper a i (toList str) ) \\ a <-: str & i<-[0..]}                                     



//Start = everySentenceWithUpper "Hi. i need your advice! currently I am FAILING every subject, so do you think I should change my major? i am so screwed."
//"Hi. I need your advice! Currently I am FAILING every subject, so do you think I should change my major? I am so screwed."
//Start = everySentenceWithUpper "oops..! I think I lost my engagement ring... do you thing he will be mad?! please tell me that he won't!"
//"Oops..! I think I lost my engagement ring... Do you thing he will be mad?! Please tell me that he won't!"
//Start = everySentenceWithUpper "functional. programming. is. THE! BEST! subject."
//"Functional. Programming. Is. THE! BEST! Subject."
//Start = everySentenceWithUpper "o" // "O" 
//Start = everySentenceWithUpper "?" //"?"
//Start = everySentenceWithUpper ""//""



//----------------------2-------------------------
/*
You are given a text in one line. You should output the same text in a following way:
only three words should be in one line, and content should be aligned in center. Only the longest line should 
be starting immediately at the wall.
Example:
Input:
"It is a truth universally acknowledged, that a single man in possession of a good fortune, 
must be in want of a wife. However little known the feelings or views of such a man may be 
on his first entering a neighbourhood, this truth is so well fixed in the minds of the surrounding 
families, that he is considered the rightful property of some one or other of their daughters."

Output:
"           It is a
truth universally acknowledged,  //-->longest line, starts from the wall, meaning there is no space between it and wall
        that a single
      man in possession
          of a good
       fortune, must be
          in want of
       a wife. However
       little known the
      feelings or views
          of such a
          man may be
         on his first
  entering a neighbourhood,
        this truth is
        so well fixed
         in the minds
      of the surrounding
      families, that he
      is considered the
     rightful property of
         some one or
        other of their
          daughters.
"
*/





Length :: [String] -> Int
Length lst = sum [length (toList (el)) \\ el <- lst]

maxLength :: [[String]] -> Int
maxLength lst = maxList [Length el \\ el <- lst]

toStr:: [String] -> String
toStr str = foldl(+++)"" str

put:: [String] -> [String]
put [] = []
put [x:xs] = [x +++ " "] ++ put xs

hput:: [String] -> [String]
hput lst = [foldr(+++)"" lst]

putSpace :: [[String]] -> [[String]]
putSpace str = [hput (put el) \\ el <- str]


spaces :: Int -> String
spaces 0 = ""
spaces n = " " +++ spaces (n-1)

helpformat :: [String] Int -> String
helpformat [] _ = ""
helpformat x max = (spaces ((max - (Length x))/2)) +++ ((toStr x))

format :: [[String]] Int -> String
format [[]] max = ""
format [x] max = (spaces ((max - (Length x))/2)) +++ toStr x
format [x:xs] max =(helpformat x max) +++ "\n" +++ (format xs max) 

ThreeWords:: [String] -> [[String]]
ThreeWords [] = []
ThreeWords [x] = [[x]]
ThreeWords [x,y] = [[x,y]]
ThreeWords [x,y,z:xs] = [[x,y,z]] ++ ThreeWords xs 


makeWord :: [Char] -> String
makeWord lst = foldr(+++)"" [toString a \\ a<-lst]

fword :: [Char] -> [Char]
fword [] = []
fword lst = tl lst

words :: [Char] -> [String]
words [] = []
words lst = [makeWord (takeWhile ((<>)' ') lst)] ++ words (fword (dropWhile ((<>)' ') lst))  



centerAlign :: String -> String
centerAlign str = (format array max) +++ "\n" 
where 
	 array = putSpace (ThreeWords (words (toList str)))
	 max = (maxLength (putSpace (ThreeWords (words [a \\ a<-: str]))))

Start = centerAlign "It is a truth universally acknowledged, that a single man in possession of a good fortune, must be in want of a wife. However little known the feelings or views of such a man may be on his first entering a neighbourhood, this truth is so well fixed in the minds of the surrounding families, that he is considered the rightful property of some one or other of their daughters."
//Start = centerAlign "CLEAN" // "CLEAN"
//Start = centerAlign "Yes, CLEAN is cool!"
/*
"Yes, CLEAN is --> the longest line at the wall
   cool!
"
*/
//Start = centerAlign "Tommy and his sister Annika had a new neighbor. Her name was Pippi Longstocking. She was nine years old, and she lived all alone in her house, Villa Villekulla."
/*
"    Tommy and his
  sister Annika had
   a new neighbor.
     Her name was
Pippi Longstocking. She --> the longest line at the wall
    was nine years
     old, and she
   lived all alone
    in her house,
  Villa Villekulla.
"
*/













