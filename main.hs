-- Animal guessing startGame
-- Think of an animal and see if the game can guess your animal.

-- The file used for a knowleadge base MUST be in the following format to be considered valid:
-- "animal_1"["prop_11",..,"prop_1i"]
-- ..
-- "animal_n"["prop_n1",..,"prop_nj"]

import AnimalGuessingGame(startGame, file_name)
import System.Directory
    
main = do
    fileExists <- doesFileExist file_name
    if fileExists then do
      putStrLn "Welcome to a new game of Guess the animal \n Think af an animal..\n Let's start guessing\n.."
      putStrLn "MENU\n *Start new game(n)\n *Quit(q)"
      key <- getChar
      case key of
          'n' -> startGame file_name
          'q' -> putStrLn "\n Thank you for playing."
          _   -> main
    else putStrLn "Error, a file missing!\n"