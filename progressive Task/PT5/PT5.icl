module PT5
import StdEnv

//REMEMBER: PLAGIARISM IS 0!

//WRITE YOUR NAME AND NEPTUN CODE HERE:

//Task:

/*
You are given a list of characters. You should output a list of strings. So, you are creating names from given
characters, every name should have length 3, should start with capital letter and every character in it should
be unique (no repetition of characters in one name). Create every possible combination of those characters!
For example if you are creating a name from this 3 characters 'a','k','t' you should have:
"Akt","Atk","Kat","Kta","Tak","Tka"
*/

getUpper:: [Char] -> [Char]
getUpper lst = [toUpper el \\ el <- lst]


helper :: Char [Char] -> [String]
helper a lst = [(toString (a)) +++ (toString el) \\ el <- lst | (toUpper a) <> el &&  a <> el]

makestr :: Char Char Char -> String
makestr a b c = (toString a) +++ (toString b) +++ (toString c)




names :: [Char] -> [String]
names lst =  flatten [(helper el1 (removeDup lst))  \\  el1 <- (getUpper (removeDup lst))] 


Start = names ['a','k','t']//["Akt","Atk","Kat","Kta","Tak","Tka"]
//Start = names ['d', 'a', 'd', 'd', 'a', 'd', 'a', 'a']//[] --> there are no 3 unique characters in the list
//Start = names ['i', 'l', 'i', 'a', 'l', 'a', 'r', 'l']//["Ila","Ilr","Ial","Iar","Irl","Ira","Lia","Lir","Lai","Lar","Lri","Lra","Ail","Air","Ali","Alr","Ari","Arl","Ril","Ria","Rli","Rla","Rai","Ral"]
