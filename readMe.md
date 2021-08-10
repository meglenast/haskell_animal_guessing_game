# Prerequisites

*  **GHCI**

    * <span style="background-color:lightgrey"> cabal user-config init -f  </span>
    * <span style="background-color:lightgrey"> choco install haskell-dev refreshenv  </span>
*  **HUnit**
    * <span style="background-color:lightgrey"> cabal install HUnit  </span>

# Run game 

* Run <span style="background-color:lightgrey"> mkdir animalGuessingGame && cd animalGuessingGame && git clone https://github.com/meglenast/haskell_animal_quessing_game.git </span>

* Compile <span style="background-color:lightgrey"> ghc main.hs </span>
* Run <span style="background-color:lightgrey"> ./main </span>

# Run tests
* Run <span style="background-color:lightgrey"> ghci </span> in project directory to enter GHC's interactive environment
* Run <span style="background-color:lightgrey"> :l Test/AnimalGuessingGameTest.hs </span>
* Run <span style="background-color:lightgrey"> main </span>