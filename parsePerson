import Data.Char

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving (Show)

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving (Show)

getJustValue (Just a) = a

deleteTwo :: [a] -> [a]

deleteTwo (x:y:xs) = xs

spliter :: String -> (String, String)
spliter str = helper ([],[]) str
								where 
									helper (key, value) [] = (reverse key, value)
									helper (key, value) (x:xs) = if (not (x == ' ')) then helper (x:key,value) xs else helper (key, deleteTwo xs) []
									
parsePerson :: String -> Either Error Person
parsePerson arr = let a = map spliter (lines arr) in 
						if any (\x -> snd x == "") a then Left ParsingError else
						if (lookup "firstName" a == Nothing || lookup "lastName" a == Nothing || lookup "age" a == Nothing) then Left IncompleteDataError else
						if (not (all isDigit (getJustValue $ lookup "age" a))) then Left (IncorrectDataError (getJustValue $ lookup "age" a)) else
						Right $ Person {firstName = getJustValue $ lookup "firstName" a, lastName = getJustValue $ lookup "lastName" a, age = read . getJustValue $ lookup "age" a}
							
