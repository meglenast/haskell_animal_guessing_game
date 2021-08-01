
module Test.AnimalGuessingGameTest where

import AnimalGuessingGame
import Test.HUnit

validFile :: String
validFile = "./Test/valid_file.txt" 

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

--Unit tests regarding the correctness of the data file validation
test_valid_file = TestCase $ do
    mockFileContent <- readFile validFile
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


test4 = TestCase $ do 
    assertBool "true is true" True
    -- assertFailure "this ffails"

-- test5 = TestCase $ do
--     1 @=? (fact 1)
--     (fact 0) @?= 1
--     True @? "True is true"
--     False @? "False is false"

t = TestList[test_valid_file, test_invalid_file_1, test_invalid_file_2, test_invalid_file_3, test_invalid_file_4, test_invalid_file_5]

main  = do 
    runTestTT t
