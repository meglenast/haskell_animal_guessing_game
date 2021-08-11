module Test.AnimalGuessingGameTest where

import AnimalGuessingGame
import Test.HUnit

emptyFile :: String
emptyFile = "./Test/TestingFiles/empty_file.txt" 

validFile_1 :: String
validFile_1 = "./Test/TestingFiles/valid_file.txt" 

validFile_2 :: String
validFile_2 = "./Test/TestingFiles/animals.txt" 

invalidFile_1 :: String
invalidFile_1 = "./Test/TestingFiles/invalid_file1.txt" 

invalidFile_2 :: String
invalidFile_2 = "./Test/TestingFiles/invalid_file2.txt" 

invalidFile_3 :: String
invalidFile_3 = "./Test/TestingFiles/invalid_file3.txt" 

invalidFile_4 :: String
invalidFile_4 = "./Test/TestingFiles/invalid_file4.txt" 

invalidFile_5 :: String
invalidFile_5 = "./Test/TestingFiles/invalid_file5.txt" 

koala :: Animal
koala = Animal "koala" ["4-legged","furry","lazy","eats-bamboo"]

panda :: Animal
panda = Animal "panda" ["4-legged","furry","eats-bamboo"]

cat :: Animal
cat = Animal "cat" ["4-legged","furry"]

dog :: Animal
dog = Animal "dog" ["4-legged","furry", "barks"]

koala_bst :: Tree String
koala_bst = (Tree {root = "4-legged", left = Tree {root = "furry", left = Tree {root = "lazy", left = Tree {root = "eats-bamboo", left = Tree {root = "koala", left = EmptyTree, right = EmptyTree}, right = EmptyTree}, right = EmptyTree}, right = EmptyTree}, right = EmptyTree})

--Unit tests regarding the correctness of the data file validation
test_valid_file1 = TestCase $ do
  mockFileContent <- readFile validFile_1
  let mockFileLines = splitIntoLines mockFileContent
  assertBool  "Valid file structure" ((validateFileAnimals mockFileLines) == True)

test_valid_file2 = TestCase $ do
  mockFileContent <- readFile validFile_2
  let mockFileLines = splitIntoLines mockFileContent
  assertBool  "Valid file structure" ((validateFileAnimals mockFileLines) == True)

test_invalid_file_1 = TestCase $ do
  mockFileContent <- readFile invalidFile_1
  let mockFileLines = splitIntoLines mockFileContent
  assertBool  "Invalid file structure - invalid text after a correct file structure fragment" (not (validateFileAnimals mockFileLines))

test_invalid_file_2 = TestCase $ do
  mockFileContent <- readFile invalidFile_2
  let mockFileLines = splitIntoLines mockFileContent
  assertBool  "Invalid file structure - invalid text before a correct file structure fragment" (not (validateFileAnimals mockFileLines))

test_invalid_file_3 = TestCase $ do
  mockFileContent <- readFile invalidFile_3
  let mockFileLines = splitIntoLines mockFileContent
  assertBool  "Invalid file structure - missed quotes in animal's name part" (not (validateFileAnimals mockFileLines))

test_invalid_file_4 = TestCase $ do
  mockFileContent <- readFile invalidFile_4
  let mockFileLines = splitIntoLines mockFileContent
  assertBool  "Invalid file structure - invalid symbols between animal's's name and animal's props" (not (validateFileAnimals mockFileLines))

test_invalid_file_5 = TestCase $ do
  mockFileContent <- readFile invalidFile_5
  let mockFileLines = splitIntoLines mockFileContent
  assertBool  "Invalid file structure - invalid animal's props structure" (not (validateFileAnimals mockFileLines))

-- Unit tests regarding the parsing of the file into list of objects of type Animal
test_parsing_file = TestCase $ do
  mockFileContent <- readFile validFile_1
  let mockFileLines = splitIntoLines mockFileContent
  assertEqual "Valid parsing" (parse mockFileLines) [(Animal "unicorn"["pink"]), (Animal "cat"["4-legged","furry","meows"])]

test_parsing_empty_file = TestCase $ do
  mockFileContent <- readFile emptyFile
  let mockFileLines = splitIntoLines mockFileContent
  assertEqual "Valid parsing empty file" (parse mockFileLines) []

-- Unit tests regarding the splitting of the file into separating the file into [String] //Works with both /r/n and /n as a new line delim
test_split_file_into_lines = TestCase $ do
  mockFileContent <- readFile validFile_2
  assertEqual "Split into lines" (splitIntoLines mockFileContent) 
    ["\"unicorn\"[\"pink\"]","\"cat\"[\"4-legged\",\"furry\",\"meows\"]","\"koala\"[\"4-legged\",\"furry\",\"lazy\",\"eats-bamboo\"]","\"dog\"[\"4-legged\",\"furry\",\"barks\"]"]

-- Unit tests regarding inserting into binary search tree 

-- Empty tree
test_insert_empty_bst = TestCase $ do
  assertEqual "Insert animal into empty BST" (insertIntoEmptyTree koala) 
    (Tree {root = "4-legged", left = Tree {root = "furry", left = Tree {root = "lazy", left = Tree {root = "eats-bamboo", left = Tree {root = "koala", left = EmptyTree, right = EmptyTree}, right = EmptyTree}, right = EmptyTree}, right = EmptyTree}, right = EmptyTree})

-- Non-empty tree
test_insert1 = TestCase $ do
  let mockAnimalsNamesLst = ["koala"]
  assertEqual "Insert animal into koala BST" (insertIntoTree koala_bst panda mockAnimalsNamesLst) 
    (Tree {root = "4-legged", left = Tree {root = "furry", left = Tree {root = "lazy", left = Tree {root = "eats-bamboo", left = Tree {root = "koala", left = EmptyTree, right = EmptyTree}, right = EmptyTree}, right = Tree {root = "eats-bamboo", left = Tree {root = "panda", left = EmptyTree, right = EmptyTree}, right = EmptyTree}}, right = EmptyTree}, right = EmptyTree})

--Test scenario : Inserting into a non-empty tree with a need to switch a leaf element with a property
test_insert2 = TestCase $ do
  let mockTree = insertIntoEmptyTree cat
  let mockAnimalsNamesLst = ["cat","dog"]
  assertEqual "Insert animal with with need to rotate leaf with property" (insertIntoTree mockTree dog mockAnimalsNamesLst) 
    (Tree {root = "4-legged", left = Tree {root = "furry", left = Tree {root = "barks", left = Tree {root = "dog", left = EmptyTree, right = EmptyTree}, right = Tree {root = "cat", left = EmptyTree, right = EmptyTree}}, right = EmptyTree}, right = EmptyTree})

test_insert3 = TestCase $ do
  mockFileContent <- readFile validFile_2
  let mockFileLines = splitIntoLines mockFileContent
  let mockAnimals = parse mockFileLines
  let bst = buildBinarySearchTree mockAnimals (animalNames mockAnimals)
  assertEqual "Insert multiple animals" bst 
    (Tree {root = "pink", left = Tree {root = "unicorn", left = EmptyTree, right = EmptyTree}, right = Tree {root = "4-legged", left = Tree {root = "furry", left = Tree 
      {root = "meows", left = Tree {root = "cat", left = EmptyTree, right = EmptyTree}, right = Tree {root = "lazy", left = Tree {root = "eats-bamboo", left = Tree {root = "koala", left = EmptyTree, right = EmptyTree}, right = EmptyTree}, right = Tree {root = "barks", left = Tree {root = "dog", left = EmptyTree, right = EmptyTree}, right = EmptyTree}}}, right = EmptyTree}, right = EmptyTree}})

-- Unit tests : util. functions
test_isLeaf1 = TestCase $ do
  assertBool "isLeaf -Leaf" (isLeaf (Tree "cat" EmptyTree EmptyTree))

test_isLeaf2 = TestCase $ do
  assertBool "isLeaf - Non-leaf tree" (not (isLeaf koala_bst))

--Unit tests : animalNames
test_animalNames1 = TestCase $ do
  assertEqual "animalNames - non-empty list" (animalNames [koala, cat, dog]) ["koala", "cat", "dog"]

test_animalNames2 = TestCase $ do
  assertEqual "animalNames - empty list" (animalNames []) []

--Unit tests : deleteByKey
test_deleteByKey1 = TestCase $ do
  assertEqual "deleteByKey - deleting existing animial from non-empty list" (deleteByKey "koala" [koala, cat, dog]) [cat, dog]

test_deleteByKey2 = TestCase $ do
  assertEqual "deleteByKey - empty list" (deleteByKey "cat" []) []

test_deleteByKey3 = TestCase $ do
  assertEqual "deleteByKey - deleting non-existing animial from non-empty list" (deleteByKey "unicorn" [koala, cat, dog]) [koala, cat, dog]

--Unit tests : getPropByKey
test_getPropByKey1 = TestCase $ do
  assertEqual "getPropByKey - getting existing animial's props from non-empty list" (getPropByKey "koala" [koala, cat, dog]) ["4-legged","furry","lazy","eats-bamboo"]

test_getPropByKey2 = TestCase $ do
  assertEqual "getPropByKey - empty list" (getPropByKey "cat" []) []

test_getPropByKey3 = TestCase $ do
  assertEqual "getPropByKey - getting non-existing animial's props from non-empty list" (getPropByKey "unicorn" [koala, cat, dog]) []

--Unit tests : alreadyExists
test_alreadyExists1 = TestCase $ do
  assertBool "alreadyExists - existing animial from non-empty list" ((alreadyExists "koala" [koala, cat, dog])) 

test_alreadyExists2 = TestCase $ do
  assertBool "alreadyExists - empty list" (not (alreadyExists "cat" []))

test_alreadyExists3 = TestCase $ do
  assertBool "alreadyExists - non-existing animial from non-empty list" (not (alreadyExists "unicorn" [koala, cat, dog]))

--Unit tests : splitByDelim
test_splitByDelim1 = TestCase $ do
  assertEqual "alreadyExists - empty string" (splitByDelim "" ',') [] 

test_splitByDelim2 = TestCase $ do
  assertEqual "alreadyExists - non-empty list" (splitByDelim "cat,dog" ',') ["cat", "dog"]

test_splitByDelim3 = TestCase $ do
  assertEqual "alreadyExists - non-empty list spliting by non-existing delimiter" (splitByDelim "cat,dog" ';') ["cat,dog"]

--Unit tests : validProp
test_validProp1 = TestCase $ do
  assertBool "validProp - empty string" (not (validProp ""))

test_validProp2 = TestCase $ do
  assertBool "validProp - non-empty valid string" (validProp "\"cat\"")

test_validProp3 = TestCase $ do
  assertBool "validProp - non-empty invalid string" (not (validProp "cat"))

--Unit tests : validPropsStr
test_validPropsStr1 = TestCase $ do
  assertBool "validPropsStr - empty string" (not (validPropsStr ""))

test_validPropsStr2 = TestCase $ do
  assertBool "validPropsStr - non-empty valid string" (validPropsStr "[\"cat\",\"dog\"]")

test_validPropsStr3 = TestCase $ do
  assertBool "validPropsStr - non-empty invalid string" (not (validProp "[cat]"))

--Unit tests : validAnimalStr
test_validAnimalStr1 = TestCase $ do
  assertBool "validAnimalStr - empty string" (not (validAnimalStr ""))

test_validAnimalStr2 = TestCase $ do
  assertBool "validAnimalStr - non-empty valid string" (validAnimalStr "\"cat\"[\"furry\",\"4-legged\"]")

test_validAnimalStr3 = TestCase $ do
  assertBool "validAnimalStr - non-empty invalid string" (not (validAnimalStr "[cat]"))

--Unit tests : parseAnimal
test_parseAnimal1 = TestCase $ do
  assertEqual "parseAnimal1" (parseAnimal "\"cat\"[\"furry\",\"4-legged\"]") (Animal "cat" ["furry", "4-legged"]) 

--Unit tests : parse
test_parse1 = TestCase $ do
  assertEqual "parse - non-empty list" (parse ["\"cat\"[\"furry\",\"4-legged\"]", "\"dog\"[\"furry\",\"4-legged\",\"barking\"]"]) [(Animal "cat" ["furry", "4-legged"]), (Animal "dog" ["furry", "4-legged", "barking"])] 

test_parse2 = TestCase $ do
  assertEqual "parse - empty list" (parse []) [] 

test_parse3 = TestCase $ do
  assertEqual "parse - non-empty list with one element" (parse ["\"cat\"[\"furry\",\"4-legged\"]"]) [(Animal "cat" ["furry", "4-legged"])] 

-- Creating TestList
test_lst_validating_file_structure = TestList [test_valid_file1, test_valid_file2, test_invalid_file_1, test_invalid_file_2, test_invalid_file_3, test_invalid_file_4, test_invalid_file_5]
test_lst_parsing = TestList [test_valid_file1, test_parsing_empty_file]
test_lst_split_into_lines = TestList [test_split_file_into_lines]
test_lst_insert_BST = TestList [test_insert_empty_bst, test_insert1, test_insert2, test_insert3]

--util. functions
test_lst_isLeaf = TestList [test_isLeaf1, test_isLeaf2]
test_lst_animalNames = TestList [test_animalNames1, test_animalNames2]
test_lst_deleteByKey = TestList [test_deleteByKey1, test_deleteByKey2, test_deleteByKey3]
test_lst_getPropByKey = TestList [test_getPropByKey1, test_getPropByKey2, test_getPropByKey3]
test_lst_alreadyExists = TestList [test_alreadyExists1, test_alreadyExists2, test_alreadyExists3]
test_lst_splitByDelim = TestList [test_splitByDelim1, test_splitByDelim2, test_splitByDelim3]
test_lst_validProp = TestList [test_validProp1, test_validProp2, test_validProp3]
test_lst_validPropsStr = TestList [test_validPropsStr1, test_validPropsStr2, test_validPropsStr3]
test_lst_validAnimalStr = TestList [test_validAnimalStr1, test_validAnimalStr2, test_validAnimalStr3]
test_lst_parseAnimal = TestList [test_parseAnimal1]
test_lst_parse = TestList[test_parse1, test_parse2, test_parse2]

main  = do
  runTestTT test_lst_validating_file_structure
  runTestTT test_lst_parsing
  runTestTT test_lst_split_into_lines
  runTestTT test_lst_insert_BST
  runTestTT test_lst_isLeaf
  runTestTT test_lst_animalNames
  runTestTT test_lst_deleteByKey
  runTestTT test_lst_getPropByKey
  runTestTT test_lst_alreadyExists
  runTestTT test_lst_splitByDelim
  runTestTT test_lst_validProp
  runTestTT test_lst_validPropsStr
  runTestTT test_lst_validAnimalStr
  runTestTT test_lst_parseAnimal
  runTestTT test_lst_parse