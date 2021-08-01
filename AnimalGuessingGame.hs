module AnimalGuessingGame where

import Prelude
import Data.List

file_name :: String
file_name = "animals.txt"

data Animal = Animal { animal_name :: String,
                        properties :: [String]} deriving (Read)

data Tree a = EmptyTree | Tree {root :: a,
                            left  :: (Tree a),
                            right :: (Tree a)} deriving (Show, Eq)
                
instance Show Animal where
    show (Animal animal_name properties) = show (animal_name) ++ show (properties)

isLeaf ::(Eq a) => Tree a -> Bool
isLeaf EmptyTree = False
isLeaf bst
    | ((left bst) == EmptyTree) && ((right bst) == EmptyTree) = True
    | otherwise = False

safeData :: [Animal] -> IO ()
safeData content = writeFile file_name . intercalate "\n" . map show $ content

loadDataFromFile :: String -> IO String
loadDataFromFile file = readFile file

animalNames:: [Animal] -> [String]
animalNames animals = map (\ curr -> (animal_name curr)) animals

deleteByKey :: String -> [Animal] -> [Animal]
deleteByKey _ [] = []
deleteByKey key (x:xs)
    | key == (animal_name x) = xs
    | otherwise = x : (deleteByKey key xs)

getPropByKey :: String -> [Animal] -> [String]
getPropByKey _ [] = []
getPropByKey curr (x:xs)
    | curr == (animal_name x) = (properties x)
    | otherwise = getPropByKey curr xs 

alreadyExists :: String -> [Animal] -> Bool
alreadyExists _ [] = False
alreadyExists curr (x:xs) 
    | curr == (animal_name x) = True
    | otherwise = alreadyExists curr xs

splitByDelim :: String -> Char -> [String]
splitByDelim "" _ = []
splitByDelim str delim = curr : splitByDelim rest delim
    where
    curr = takeWhile (/= delim) str
    temp = dropWhile (/= delim) str
    rest = if (temp == "") then temp  else (tail temp)

validProp :: String -> Bool
validProp str
    | length str <= 2 = False
    | ((head str) == '"') && ((last str) == '"') = True
    | otherwise = False

validPropsStr :: String -> Bool
validPropsStr str
    | length str <= 4 = False
    |  ((head str) == '[')  && ((last str) == ']') && (all (\x -> (validProp x)) props) = True
    |  otherwise  = False
    where
    props = splitByDelim (init (tail str)) ','

validAnimalStr :: String -> Bool
validAnimalStr str
    | length str <= 8 = False
    | (validNameStr nameStr) && (validPropsStr propsStr) = True
    | otherwise = False
    where
    nameStr  = (takeWhile (/= '[') str)
    propsStr = (dropWhile (/= '[') str)

parseAnimal :: String -> Animal
parseAnimal str = Animal name propsLst
    where
    name  = (init (tail (takeWhile (/= '[') str)))
    props = splitByDelim (init (tail (dropWhile (/= '[') str))) ','
    propsLst = map (\ x -> (init (tail x))) props


parse:: [String] -> [Animal]
parse [] = []
parse (x:xs) = (parseAnimal x) : (parse xs)
    
validateFileAnimals:: [String] -> Bool
validateFileAnimals [] = False
validateFileAnimals xs = (all (\ x -> validAnimalStr x) xs)  

isEndOfLine :: Char -> Bool
isEndOfLine '\n' = True
isEndOfLine '\r' = True
isEndOfLine _ = False

splitIntoLines :: String -> [String]
splitIntoLines "" = []
splitIntoLines str = if (curr == "") then splitIntoLines trimmed else curr : splitIntoLines trimmed 
    where
        curr = takeWhile (\ symbol -> (not (isEndOfLine symbol))) str
        rest = dropWhile (\ symbol -> (not (isEndOfLine symbol))) str
        trimmed = if (rest /= "") then (tail rest) else rest

insertIntoEmptyTree :: Animal -> Tree String
insertIntoEmptyTree animal
    | props == [] = Tree name EmptyTree EmptyTree
    | otherwise = Tree (head props) (insertIntoEmptyTree (Animal name (tail props))) EmptyTree
    where
        name = animal_name animal
        props = properties animal

insertIntoTree :: Tree String -> Animal -> [String] -> Tree String
insertIntoTree bst animal names 
    | (root bst) `elem` props = Tree (root bst) (insertBST (left bst) (Animal name (delete (root bst) props)) names) (right bst)
    | (root bst) `elem` names = Tree (head props) (insertBST (left bst) (Animal name (tail props)) names)  (Tree (root bst) EmptyTree EmptyTree) 
    | otherwise =  Tree (root bst) (left bst) (insertBST (right bst) (Animal name (delete (root bst) props)) names)
    where
        name = animal_name animal
        props = properties animal

insertBST :: Tree String -> Animal -> [String] -> Tree String
insertBST EmptyTree animal _ = insertIntoEmptyTree animal
insertBST bst animal names = insertIntoTree bst animal names

buildBinarySearchTree :: [Animal] -> [String] -> Tree String
buildBinarySearchTree [] _ = EmptyTree
buildBinarySearchTree xs names = foldl (\ tree curr -> insertBST tree curr names) EmptyTree xs

startGame :: String -> IO()
startGame filename = do
    dataAnimals <- readFile filename
    let fileLines = splitIntoLines dataAnimals
    if (validateFileAnimals fileLines) then do
        let animals = parse fileLines
        let names = animalNames animals
        let bst = buildBinarySearchTree animals names
        resolve bst [] animals
    else  putStrLn "Invalid args..Could not parse the files..\n"
    

unableToGuess :: [String] -> [Animal] -> IO ()
unableToGuess satisfied animals = do
    putStrLn "Not enough data to quess..\nMake me smarter and answer those questtions..\nWhat was your animal?.."
    userAnimal <- getLine
    putStrLn ("\n How can I recognize it? Tell me a true fact about it, please.. \n")
    userProp <- getLine
    safeData ((Animal userAnimal (userProp:satisfied)):animals)

makeGuess :: String -> [String] -> [Animal] -> IO ()
makeGuess curr_guess satisfied animals = do
    putStrLn ("\nIs your animal a/an .. " ++ curr_guess ++ "?Y/N")
    userInput <- getChar 
    if (userInput == 'Y') then putStrLn "\nI won! :) \n" 
    else do
    putStrLn ("What was your animal?..")
    userAnimal <- getLine 
    putStrLn ("\nHow can I recognize it from " ++ curr_guess ++ "?\n")
    userProp <- getLine
    putStrLn ("\nWhich one is " ++ userProp ++ "?\n")
    userAnswerCorrect <- getLine
    
    if (curr_guess == userAnswerCorrect)
    then safeData ((Animal userAnimal satisfied) : (Animal curr_guess (userProp : satisfied)) : (deleteByKey  curr_guess animals))
    else do
        if (alreadyExists userAnswerCorrect animals) 
        then do
        safeData ((Animal userAnimal ((userProp : satisfied) ++ (getPropByKey userAnswerCorrect animals))) : (deleteByKey  userAnswerCorrect animals))
        else do
        safeData ((Animal userAnimal (userProp : satisfied)) : animals)

resolve :: Tree String -> [String] -> [Animal] -> IO ()
resolve EmptyTree satisfied animals = unableToGuess satisfied animals
resolve bst satisfied animals 
    | isLeaf bst =  makeGuess (root bst) satisfied animals
    | otherwise = do
        putStrLn ("Is your animal " ++ (root bst) ++ " ? Y/N..")
        userInput <- getChar
        case userInput of
            'Y' -> resolve (left bst) (root bst : satisfied) animals
            'N' -> resolve (right bst) satisfied animals
            _   -> resolve bst satisfied animals 

allNonCapitalLetters :: String -> Bool
allNonCapitalLetters str = all (\ letter -> letter `elem` ['a'..'z']) str

validNameStr :: String -> Bool
validNameStr str
    | length str <= 2 = False
    | ((head str) == '"') && ((last str) == '"') && (allNonCapitalLetters (init (tail str))) = True
    | otherwise = False