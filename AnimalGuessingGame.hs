module AnimalGuessingGame where

import Data.List(intercalate, delete, nub)

file_name :: String
file_name = "animals.txt"

data Animal = Animal { animal_name :: String,
                        properties :: [String]} deriving (Read,Eq)

data Tree a = EmptyTree | Tree {root :: a,
                            left  :: (Tree a),
                            right :: (Tree a)} deriving (Show, Eq)
                
instance Show Animal where
    show (Animal animal_name properties) = show (animal_name) ++ show (properties)

-- Checks whether a tree is a leaf
isLeaf ::(Eq a) => Tree a -> Bool
isLeaf (Tree _ EmptyTree EmptyTree) = True 
isLeaf _ = False

-- Saves knowleadge base in the game data file
saveData :: [Animal] -> IO ()
saveData content = writeFile file_name . intercalate "\n" . map show $ content

-- Extracts a list of animal names from a given list of animals
animalNames:: [Animal] -> [String]
animalNames animals = map (\ curr -> (animal_name curr)) animals

-- Deletes an animal from a list of Animals by animal's name
deleteByKey :: String -> [Animal] -> [Animal]
deleteByKey name animals = foldr (\ curr res -> if (animal_name curr == name) then res else curr : res) [] animals

-- Gets an animal's properties from a list of Animals by animal's name    
getPropByKey :: String -> [Animal] -> [String]
getPropByKey _ [] = []
getPropByKey name animals = if res == [] then [] else properties (head  res)
  where res = dropWhile (\ curr -> (animal_name curr) /= name) animals

-- Checks whether an animal exists in a list of animals 
alreadyExists :: String -> [Animal] -> Bool
alreadyExists name  animals = any (\ curr -> (animal_name curr) == name) animals

-- Splits a string by a delimiter    
splitByDelim :: String -> Char -> [String]
splitByDelim "" _ = []
splitByDelim str delim = curr : splitByDelim rest delim
  where
  (curr, temp) = span (/= delim) str
  rest = if (temp == "") then temp  else (tail temp)

-- Validates whether a string is a valid animal's property/name representation
validProp :: String -> Bool
validProp str
  | length str <= 2 = False
  | ((head str) == '"') && ((last str) == '"') = True
  | otherwise = False

-- Validates whether a string is a valid animal's properties representation
validPropsStr :: String -> Bool
validPropsStr str
  | length str <= 4 = False
  |  ((head str) == '[')  && ((last str) == ']') && (all (\x -> (validProp x)) props) = True
  |  otherwise  = False
  where
    props = splitByDelim (init (tail str)) ','

-- Validates whether a string is a valid animal representation
validAnimalStr :: String -> Bool
validAnimalStr str
  | length str <= 8 = False
  | (validProp nameStr) && (validPropsStr propsStr) = True
  | otherwise = False
  where
    (nameStr, propsStr) = span (/= '[') str 

-- Parses valid animal string representation into Animal data type
parseAnimal :: String -> Animal
parseAnimal str = Animal name propsLst
  where
  (nameStr,propsStr) = span (/= '[') str
  name  = (init (tail nameStr))
  props = splitByDelim (init (tail propsStr)) ','
  propsLst = map (\ x -> (init (tail x))) props

-- Parses a list of valid animal's sting representations into list of objects of datatype Animal. If there are multiple animals with the same name in the file the functions adds to the resulting list the
-- animal that ocurrs first in the file.
parse:: [String] -> [Animal]
parse xs = foldl (\ res curr -> if (alreadyExists (animal_name (parseAnimal curr)) res) then res else res ++ [parseAnimal curr]) [] xs

-- Validates the file containg the knowleadge base 
validateFileAnimals:: [String] -> Bool
validateFileAnimals [] = False
validateFileAnimals xs = (all (\ x -> validAnimalStr x) xs)  

-- Checks whether a symbol is an end of file symbol \\ Problem occured when build-in function lines was used on Windows
isEndOfLine :: Char -> Bool
isEndOfLine '\n' = True
isEndOfLine '\r' = True
isEndOfLine _ = False

-- Splits string into list of strings by eof delim.
splitIntoLines :: String -> [String]
splitIntoLines "" = []
splitIntoLines str = if (curr == "") then splitIntoLines trimmed else curr : splitIntoLines trimmed 
  where
    (curr,rest) = span (\ symbol -> (not (isEndOfLine symbol))) str 
    trimmed = if (rest /= "") then (tail rest) else rest

-- Inserts an animal into empty bst
insertIntoEmptyTree :: Animal -> Tree String
insertIntoEmptyTree (Animal name props)
  | props == [] = Tree name EmptyTree EmptyTree
  | otherwise = Tree (head props) (insertIntoEmptyTree (Animal name (tail props))) EmptyTree

-- Inserts an animal into non-empty bst
insertIntoTree :: Tree String -> Animal -> [String] -> Tree String
insertIntoTree (Tree root left right) (Animal name props) names 
  | root `elem` props = Tree root (insertBST left (Animal name (delete root props)) names) right
  | root `elem` names = Tree (head props) (insertBST left (Animal name (tail props)) names)  (Tree root EmptyTree EmptyTree) 
  | otherwise =  Tree root left (insertBST right (Animal name (delete root props)) names)

-- Insets an animal into bst
insertBST :: Tree String -> Animal -> [String] -> Tree String
insertBST EmptyTree animal _ = insertIntoEmptyTree animal
insertBST bst animal names = insertIntoTree bst animal names

-- Builds bst from list of animals
buildBinarySearchTree :: [Animal] -> [String] -> Tree String
buildBinarySearchTree [] _ = EmptyTree
buildBinarySearchTree xs names = foldl (\ tree curr -> insertBST tree curr names) EmptyTree xs   

-- Asks additional questions to enrich the knowleadge base when the programm can not make a guess
unableToGuess :: [String] -> [Animal] -> IO ()
unableToGuess satisfied animals = do
  putStrLn "\nNot enough data to quess, I give up..\nMake me smarter and answer those questtions..\nWhat was your animal?.."
  userAnimal <- getLine
  putStrLn ("\nHow can I recognize it? Tell me a true fact about it, please.. \n")
  userProp <- getLine
  if (userAnimal `elem` (animalNames animals)) then  do 
    let res_prop = nub $ ((userProp : satisfied) ++ (getPropByKey userAnimal animals))
    saveData $ (Animal userAnimal res_prop):(deleteByKey userAnimal animals)
  else 
    saveData $ (Animal userAnimal (userProp:satisfied)):animals
  putStrLn ("\nThank you, I've finished writing in file.. \n")

-- Makes a guess based on the user's answers so far
makeGuess :: String -> [String] -> [Animal] -> IO ()
makeGuess curr_guess satisfied animals = do
  putStrLn ("\nIs your animal a/an .. " ++ curr_guess ++ "?yes/no")
  userInput <- getLine
  case userInput  of
    "yes" -> putStrLn "\nI won! :) \n"
    "no" -> do
      putStrLn ("\nWhat was your animal?..")
      userAnimal <- getLine 
      putStrLn ("\nHow can I recognize it from " ++ curr_guess ++ "?\n")
      userProp <- getLine
      putStrLn ("\nWhich one is " ++ userProp ++ "?\n")
      userAnswerCorrect <- getLine
    
      if (curr_guess == userAnswerCorrect)
      then saveData $ (Animal userAnimal satisfied) : (Animal curr_guess (userProp : satisfied)) : (deleteByKey  curr_guess animals)
      else do
        if alreadyExists userAnswerCorrect animals 
        then do
        saveData $ (Animal userAnimal ((userProp : satisfied) ++ (getPropByKey userAnswerCorrect animals)) : (deleteByKey  userAnswerCorrect animals))
        else do
        saveData $ (Animal userAnimal (userProp : satisfied)) : animals
    _ -> makeGuess curr_guess satisfied animals  

-- Resolves an animal based guess based on the user's answers 
resolve :: Tree String -> [String] -> [Animal] -> IO ()
resolve EmptyTree satisfied animals = unableToGuess satisfied animals
resolve bst@(Tree root left right) satisfied animals 
  | isLeaf bst =  makeGuess root satisfied animals
  | otherwise = do
    putStrLn ("\nIs your animal " ++ root ++ " ? yes/no..")
    userInput <- getLine
    case userInput of
        "yes" -> resolve left (root : satisfied) animals
        "no" -> resolve right satisfied animals
        _ -> resolve bst satisfied animals 

-- Starts a new game    
startGame :: String -> IO()
startGame filename = do
  dataAnimals <- readFile filename
  let fileLines = splitIntoLines dataAnimals
  if (validateFileAnimals fileLines) then do
    let animals = parse fileLines
    let names = animalNames animals
    let bst = buildBinarySearchTree animals names
    resolve bst [] animals
  else  putStrLn "\nInvalid args..Could not parse the files..\n"