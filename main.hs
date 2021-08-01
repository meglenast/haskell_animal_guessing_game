import AnimalGuessingGame
    
main = do
    putStrLn "Welcome to a new game of Guess the animal \n Think af an animal..\n Let's start guessing\n.."
    putStrLn "MENU\n *Start new game(n/N)\n *Quit(q/Q)"
    key <- getChar
    case key of
        'n' -> startGame "animals.txt"
        'N' -> startGame "animals.txt"
        'q' -> putStrLn "\n Thank you for playing."
        'Q' -> putStrLn "\n Thank you for playing."
        _   -> main