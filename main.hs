--Animal guessing startGame
--Think of an animal and see if the game can guess your animal.

import AnimalGuessingGame(startGame)
    
main = do
    putStrLn "Welcome to a new game of Guess the animal \n Think af an animal..\n Let's start guessing\n.."
    putStrLn "MENU\n *Start new game(n/N)\n *Quit(q/Q)"
    key <- getChar
    case key of
        'n' -> startGame "animals.txt"
        'q' -> putStrLn "\n Thank you for playing."
        _   -> main