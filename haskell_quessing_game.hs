import Prelude
import Data.List

-- --TO-DO -- create propper data structure
data Animal = Animal { animal_name :: String,
                        properties :: [String]} deriving (Read)
                
instance Show Animal where
    show (Animal animal_name properties) = show (animal_name) ++ show (properties)
        
cat :: Animal
cat = Animal "cat" ["4-legged", "furry", "meows"]

coala :: Animal
coala = Animal "coala" ["4-legged", "furry", "lazy", "eats-bamboo"]

dog :: Animal
dog = Animal "dog" ["4-legged", "furry", "barks"]

dog1 :: Animal
dog1 = Animal "dog1" ["4-legged", "furry", "barks"]

dataset :: [Animal]
dataset = [cat, coala, dog]

loadPropertiesSet :: [Animal] -> [String]
loadPropertiesSet [] = []
loadPropertiesSet xs = foldl  (\ acc curr -> acc ++ getNewProps acc (properties curr)) [] xs
    where getNewProps curr_props new_props = filter (\ x -> not (x `elem` curr_props)) new_props

readyToGuess :: [Animal] -> Bool
readyToGuess animals =  any (\ curr -> null (properties curr)) animals

makeGuess :: [Animal] -> String
makeGuess animals =  animal_name $ head $ filter (\ curr -> null (properties curr)) animals

reduceProperties :: [Animal] -> String -> [Animal]
reduceProperties [] _ = []
reduceProperties xs has_property = map (\ curr -> (dropProp has_property curr)) xs  

reduceAnimals :: [Animal] -> String -> [Animal]
reduceAnimals [] _ = []
reduceAnimals xs has_property = filter (\ x -> not(has_property `elem` (properties x))) xs

dropProp :: String -> Animal -> Animal
dropProp has_property animal = Animal (animal_name animal) (dropStr has_property (properties animal))

dropStr :: String -> [String] -> [String]
dropStr _ [] = []
dropStr str (x:xs)
    | x == str = xs
    | otherwise = x : dropStr str xs

unableToGuess :: [Animal] -> [String] -> IO()
unableToGuess animals satisfied = do
    putStrLn "Not enough data to quess..\nMake me smarter and answer those questtions..\nWhat was your animal?.."
    userAnimal <- getLine
    putStrLn "Tell me a true fact about it.\nHow can I recognize it?"
    userProp <- getLine
    putStrLn "FINISH WRitiNG iN FILE"
    -- writeFile "animals.txt" ((Animal userAnimal (userProp:satisfied)):animals)
    writeFile "animals.txt" . intercalate "\n" . map show $ ((Animal userAnimal (userProp:[])):animals)
   
makeProbableGuess :: [Animal] -> String
makeProbableGuess [] = " "
makeProbableGuess xs = animal_name probable_guess
    where probable_guess = foldl (\ res curr -> (if (length (properties curr) < length (properties curr)) then curr else res )) (head xs) xs

ableToGuess :: [Animal] -> [Animal] -> [String] -> IO ()
ableToGuess prevDataBase currAnimals satisfied = do
   putStrLn $ "I'll try to make a quess..\nWas your animal a/an..." ++ (makeProbableGuess currAnimals) ++ " ?"

ask :: [Animal] -> [Animal] -> [String] -> [String]  -> IO ()
ask currAnimals animals [] satisfied = do ableToGuess currAnimals animals satisfied
ask currAnimals [] _ satisfied = do unableToGuess currAnimals satisfied
ask currAnimals animals properties satisfied
    | readyToGuess animals = do putStrLn $ makeGuess animals
    | otherwise = do
        putStrLn ("Is your animal " ++ (head properties) ++ " ? Y/N..")
        userInput <- getLine
        if userInput ==  "Y" then --to-do case instead
            ask currAnimals (reduceProperties animals (head properties)) (tail properties) (head properties : satisfied)
        else 
            ask currAnimals (reduceAnimals animals (head properties)) (tail properties) satisfied
 
loadDataFromFile :: String -> IO String
loadDataFromFile file = readFile file

allNonCapitalLetters :: String -> Bool
allNonCapitalLetters str = all (\ letter -> letter `elem` ['a'..'z']) str

validNameStr :: String -> Bool
validNameStr str
    | length str <= 2 = False
    | ((head str) == '"') && ((last str) == '"') && (allNonCapitalLetters (init (tail str))) = True
    | otherwise = False


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

validateFileQuestions:: [String] -> Bool
validateFileQuestions [] = False
validateFileQuestions xs = (all (\ x -> validProp x) xs)  

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

startGame :: [String] -> IO()
startGame fileLinesAnimals = do
    let animals = parse fileLinesAnimals
    let questions = loadPropertiesSet animals    
    ask animals animals questions []

main = do
    putStrLn "Welcome to a new game of Guess the animal \n Think af an animal..\n Let's start quessing\n.."  
    dataAnimals <- readFile "animals.txt"
    dataQuestions <- readFile "questions.txt"
    let fileLines = splitIntoLines dataAnimals
    let questionsLines = splitIntoLines dataQuestions
    putStrLn dataQuestions 
    if ((validateFileAnimals fileLines) && (validateFileQuestions questionsLines))
    then startGame fileLines   
    else  putStrLn "Invalid args..Could not parse the files..\n"
    putStrLn "End..\n.."