import Prelude

-- --TO-DO -- create propper data structure
data Animal = Animal { animal_name :: String,
                        properties :: [String]} deriving Show
        
cat :: Animal
cat = Animal "cat" ["4-legged", "furry", "meows"]

coala :: Animal
coala = Animal "coala" ["4-legged", "furry", "lazy", "eats-bamboo"]
-- coala = Animal "coala" []

dog :: Animal
dog = Animal "dog" ["4-legged", "furry", "barks"]
-- dog = Animal "dog" []

dog1 :: Animal
dog1 = Animal "dog1" ["4-legged", "furry", "barks", "big"]
-- dog1 = Animal "dog1" []

-- --TO-DO -- function that loads animals from file int a list of Animals
dataset :: [Animal]
dataset = [cat, coala, dog, dog1]

-- --TO-DO -- funcition that loads the properties into list of properties
loadPropertiesSet :: [Animal] -> [String]
loadPropertiesSet [] = []
loadPropertiesSet xs = foldl  (\ acc curr -> acc ++ getNewProps acc (properties curr)) [] xs
    where getNewProps curr_props new_props = filter (\ x -> not (x `elem` curr_props)) new_props

-- --TO-DO --ready to guess
readyToGuess :: [Animal] -> Bool
readyToGuess animals =  any (\ curr -> null (properties curr)) animals

-- --TO-DO --make a quessing
makeGuess :: [Animal] -> String
makeGuess animals =  animal_name $ head $ filter (\ curr -> null (properties curr)) animals

-- --TO-DO --reduce Animals
reduceProperties :: [Animal] -> String -> [Animal]
reduceProperties [] _ = []
reduceProperties xs has_property = map (\ curr -> (dropProp has_property curr)) xs  

filterProperties :: [Animal] -> String -> [Animal]
filterProperties [] _ = []
filterProperties xs has_property = map (\ curr -> (dropProp has_property curr)) xs

-- --TO-DO -- dropProp
dropProp :: String -> Animal -> Animal
dropProp has_property animal = Animal (animal_name animal) (dropStr has_property (properties animal))
-- --TO-DO -- dropping string cuz the one inghc is only for integers first occurence
dropStr :: String -> [String] -> [String]
dropStr _ [] = []
dropStr str (x:xs)
    | x == str = xs
    | otherwise = x : dropStr str xs
-- --TO-DO -- ask question
-- ask :: [Animal] -> [String] -> IO ()
-- ask  animals properties = ask' animals properties []
    -- where 

ask' :: [Animal] -> [String] -> [String]  -> IO ()
ask' animals [] _ = do putStrLn "TO-DO-GUESSING-FUNCS"
ask' animals properties satisfied
    | readyToGuess animals = do putStrLn $ makeGuess animals
    | otherwise = do
        putStrLn ("Is your animal " ++ (head properties) ++ " ? Y/N..")
        userInput <- getLine
        if userInput ==  "Y" then
            ask'  (reduceProperties animals (head properties)) (tail properties) (head properties : satisfied)
        else 
            ask' animals (tail properties) satisfied

-- main = do
--     putStrLn "Welcome to a new game of Guess the animal \n Think af an animal..\n Let's start quessing\n.."
--     let props = loadPropertiesSet dataset
--     let loadedData = dataset
--     -- ask loadedData props 
--     putStrLn "Welcome to a new game of Guess the animal \n Think af an animal..\n Let's start quessing\n.."
