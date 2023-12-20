module HW10
import StdEnv

//Please write your name and neptun code here: <  Abaidullah Asif  ,  Zlwd2b  >
//PLAGIARISM IS 0!
//Main point of this tasks is for you to practice - instances!

::Movie = {
			title::String,
			casts::[Cast],
			year::Int,
			rating::Real,
			country::String
			}
::Gender = Male | Female
::Cast = {
			name::String,
			gender::Gender
			}

cast1 = { name="Jackie Chan", gender= Male}
cast2 = { name="Jet Li", gender= Male }
cast3 = { name="Millie Bobby Brown", gender=Female}
cast4 = { name="Chris Hemsworth", gender=Male }
cast5 = { name="Zendaya", gender=Female }
cast6 = { name="Emma Stone", gender=Female }
cast7 = { name="Emma Watson", gender = Female }
cast8 = { name="Sandra Bullock", gender = Female}
cast9 = { name="Chris Evans", gender= Male}
cast10 = { name="Tom Holland", gender=Male}
cast11 = { name="Tobey Maguire", gender=Male}
cast12 = { name="Bae Suzy", gender=Female}
cast13 = { name="Park Seo Joon", gender=Male}

movie1 = { title="MOVIE I.", casts=[cast1, cast6, cast10], year=2019, rating=8.5, country="USA"}
movie2 = { title="MOVIE II.", casts=[cast4, cast8, cast7], year=2020, rating=8.0, country="Spain"}
movie3 = { title="MOVIE III.", casts=[cast13, cast12, cast9], year=2019, rating=9.0, country="Korea"}
movie4 = { title="MOVIE IV.", casts=[cast3, cast11, cast8], year=2021, rating=6.5, country="India"}
movie5 = { title="MOVIE V.", casts=[cast2, cast4, cast10], year=2022, rating=7.4, country="Hungary"}
movie6 = { title="MOVIE VI.", casts=[cast3, cast5, cast8], year=2022, rating=7.4, country="Hungary"}

movieList = [movie1,movie2,movie3,movie4,movie5, movie6]



/* ----------------------------------------TASK 1 - 75 POINTS: -----------------------------------------------------
	You get list of movies and you should output a list of tuple of 3 boleean values.
	 
	FIRST boolean shows True - if movie's rating was higher than the average of all movie ratings
	For example: average of ratings of every movie is 7.8 and it is less than ratings of movie1(8.5)
	
	SECOND boolean shows True - if every cast in current movies' list of casts was unique - meaning that
	that cast didn't participate in any other movie in the list of given movies.
	For example: movie1 had 3 casts: cast1, cast6, cast10. It will return False, cause cast 10 also participated
	in movie5
	
	THIRD boolean shows True - if male actors in the movie were more than females. So for every cast in casts
	you should check genders of casts and check if males > females
	
	The solution should not rely on the fact that you know the list of casts.
	
	****HINT: you need to define == instance on Gender and on Cast
	
*/


areCastsEqual :: [Cast] [Cast] -> Bool
areCastsEqual lst1 lst2 = (length [el \\ el <- lst1 | isMember el lst2] == length lst1)

instance == Movie
where (==) a b = a.title == b.title && (areCastsEqual a.casts b.casts) == True && a.rating == b.rating && a.country == b.country && a.year == b.year

instance < Movie
where (<) a b = a.year < b .year

instance == Gender
where 
	(==) Male Male = True
	(==) Female Female = True
	(==) Male Female = False
	(==) Female Male = False


instance == Cast
where (==) a b = (a.name == b.name) && (a.gender == b.gender)


checkCast :: Movie [Movie] -> Bool
checkCast m mlst = length [el \\ el <- m.casts | not (isMember el castlist)] == (length m.casts)

where castlist = removeDup (flatten [el.casts \\ el <- mlst | el <> m])


checkMale :: Movie -> Bool
checkMale m = length [el \\ el <- m.casts | el.gender == Male]  > length [el \\ el <- m.casts | el.gender == Female]

checkThree :: [Movie] -> [(Bool, Bool, Bool)]
checkThree lst = [(el.rating > z, checkCast el lst, checkMale el) \\ el <- lst]
where z = (sum [el.rating \\ el <- lst]) / toReal (length lst)

//Start = checkThree movieList
//[(True,False,True),(True,False,False),(True,True,True),(False,False,False),(False,False,True),(False,False,False)]


/* ----------------------------------------TASK 2 - 25 POINTS: ----------------------------------------------------

Sort the same list of movies according to the year it was released. Output only the title of movies of sorted list.

*/


sortedMoviesTitles :: [Movie] -> [String]
sortedMoviesTitles mlst = [el.title \\ el <- z]
where z = sortBy (<) mlst
Start = sortedMoviesTitles movieList
//["MOVIE I.","MOVIE III.","MOVIE II.","MOVIE IV.","MOVIE V.","MOVIE VI."]




















