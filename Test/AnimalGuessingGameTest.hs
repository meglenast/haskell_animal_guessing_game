module Test.AnimalGuessingGameTest where

import AnimalGuessingGame
import Test.HUnit

emptyFile :: String
emptyFile = "./Test/empty_file.txt" 

validFile_1 :: String
validFile_1 = "./Test/valid_file.txt" 

validFile_2 :: String
validFile_2 = "./Test/animals.txt" 

invalidFile_1 :: String
invalidFile_1 = "./Test/invalid_file1.txt" 

invalidFile_2 :: String
invalidFile_2 = "./Test/invalid_file2.txt" 

invalidFile_3 :: String
invalidFile_3 = "./Test/invalid_file3.txt" 

invalidFile_4 :: String
invalidFile_4 = "./Test/invalid_file4.txt" 

invalidFile_5 :: String
invalidFile_5 = "./Test/invalid_file5.txt" 

koala :: Animal
koala = Animal "koala" ["4-legged","furry","lazy","eats-bamboo"]

panda :: Animal
panda = Animal "panda" ["4-legged","furry","eats-bamboo"]

koala_bst :: Tree String
koala_bst = (Tree {root = "4-legged", left = Tree {root = "furry", left = Tree {root = "lazy", left = Tree {root = "eats-bamboo", left = Tree {root = "koala", left = EmptyTree, right = EmptyTree}, right = EmptyTree}, right = EmptyTree}, right = EmptyTree}, right = EmptyTree})
-- invalidFile_5 :: String
-- invalidFile_5 = "./Test/invalid_file5.txt" 

--Unit tests regarding the correctness of the data file validation
test_valid_file = TestCase $ do
    mockFileContent <- readFile validFile_1
    let mockFileLines = splitIntoLines mockFileContent
    assertBool  "Valid file structure" ((validateFileAnimals mockFileLines) == True)

test_invalid_file_1 = TestCase $ do
    mockFileContent <- readFile invalidFile_1
    let mockFileLines = splitIntoLines mockFileContent
    assertBool  "Invalid file structure - invalid text after a correct file structure fragment" ((validateFileAnimals mockFileLines) == False)

test_invalid_file_2 = TestCase $ do
    mockFileContent <- readFile invalidFile_2
    let mockFileLines = splitIntoLines mockFileContent
    assertBool  "Invalid file structure - invalid text before a correct file structure fragment" ((validateFileAnimals mockFileLines) == False)

test_invalid_file_3 = TestCase $ do
    mockFileContent <- readFile invalidFile_3
    let mockFileLines = splitIntoLines mockFileContent
    assertBool  "Invalid file structure - missed quotes in animal's name part" ((validateFileAnimals mockFileLines) == False)

test_invalid_file_4 = TestCase $ do
    mockFileContent <- readFile invalidFile_4
    let mockFileLines = splitIntoLines mockFileContent
    assertBool  "Invalid file structure - invalid symbols between animal's's name and animal's props" ((validateFileAnimals mockFileLines) == False)

test_invalid_file_5 = TestCase $ do
    mockFileContent <- readFile invalidFile_5
    let mockFileLines = splitIntoLines mockFileContent
    assertBool  "Invalid file structure - invalid animal's props structure" ((validateFileAnimals mockFileLines) == False)

-- Unit tests regarding the parsing of the file into list of objects of type Animal
test_parsing_file = TestCase $ do
    mockFileContent <- readFile validFile_1
    let mockFileLines = splitIntoLines mockFileContent
    assertEqual "Valid parsing" (parse mockFileLines) [(Animal "unicorn"["pink"]), (Animal "cat"["4-legged","furry","meows"])]

test_parsing_empty_file = TestCase $ do
    mockFileContent <- readFile emptyFile
    let mockFileLines = splitIntoLines mockFileContent
    assertEqual "Valid parsing" (parse mockFileLines) []

-- Unit tests regarding the splitting of the file into separating the file into [String] //Works with both /r/n and /n as a new line delim
test_split_file_into_lines = TestCase $ do
    mockFileContent <- readFile validFile_2
    assertEqual "Valid parsing" (splitIntoLines mockFileContent) ["\"unicorn\"[\"pink\"]","\"cat\"[\"4-legged\",\"furry\",\"meows\"]","\"koala[\"4-legged\",\"furry\",\"lazy\",\"eats-bamboo\"]","\"dog\"[\"4-legged\",\"furry\",\"barks\"]"]

-- Unit tests regarding inserting into binary search tree curr

-- Empty tree
test_insert_empty_bst = TestCase $ do
    assertEqual "Insert animal into empty BST" (insertIntoEmptyTree koala) (Tree {root = "4-legged", left = Tree {root = "furry", left = Tree {root = "lazy", left = Tree {root = "eats-bamboo", left = Tree {root = "koala", left = EmptyTree, right = EmptyTree}, right = EmptyTree}, right = EmptyTree}, right = EmptyTree}, right = EmptyTree})

-- Non-empty tree
test_insert_koala_bst = TestCase $ do
    let mockAnimalsNamesLst = ["koala"]
    assertEqual "Insert animal into koala BST" (insertIntoTree koala_bst panda mockAnimalsNamesLst) (Tree {root = "4-legged", left = Tree {root = "furry", left = Tree {root = "lazy", left = Tree {root = "eats-bamboo", left = Tree {root = "koala", left = EmptyTree, right = EmptyTree}, right = EmptyTree}, right = Tree {root = "eats-bamboo", left = Tree {root = "panda", left = EmptyTree, right = EmptyTree}, right = EmptyTree}}, right = EmptyTree}, right = EmptyTree})

-- Creating TestList
test_lst_validating_file_structure = TestList [test_valid_file, test_invalid_file_1, test_invalid_file_2, test_invalid_file_3, test_invalid_file_4, test_invalid_file_5]
test_lst_parsing = TestList [test_valid_file, test_parsing_empty_file]
test_lst_split_into_lines = TestList [test_split_file_into_lines]
test_lst_insert_BST = TestList [test_insert_empty_bst, test_insert_koala_bst]

main  = do
    -- let res = insertIntoEmptyTree koala
    -- putStrLn (show res)
    runTestTT test_lst_validating_file_structure
    runTestTT test_lst_parsing
    runTestTT test_lst_split_into_lines
    runTestTT test_lst_insert_BST
