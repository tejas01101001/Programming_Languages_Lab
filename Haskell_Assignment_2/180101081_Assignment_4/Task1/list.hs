{-
    Author : Tejas Khairnar
    Roll number : 180101081

    1. The code was written ,complied and executed in Ubuntu 20.04 using
       Glasgow Haskell Compiler, Version 8.6.5, stage 2 booted by GHC version 8.6.5
    2. Make sure you have ghc installed on your system
    3. Change the working directory to the source code
    4. To compile the code and create the object file type the following command in terminal
      'ghc -o obj list.hs'
    5. To run the code type the following command in terminal
      './obj'
    6. Enter the comma seperated input list (without square brackets)
    7. I have also given 10 testcases at the end of the code for which you can test the code
    8. The time complexity of this particular code is O(n) due to the list traversal

-}

import Data.List ()
import Data.List.Split (splitOn)
import System.IO ()


main :: IO ()
main = do
  putStrLn "Enter the comma seperated input list (without square brackets)"

  -- Read the comma seperated input 
  s <- getLine

  -- Split the string at the deliminater comma i.e. ','
  -- And convert the each part of string to Int
  let list = map (read :: String -> Int) (splitOn "," s)  
  

  putStr "The actual list after parsing is "
  print list

------------ TESTCASES ----------------
{-
         Input              Output
    1.   192                 [192]
    2.   1,9,10              [1,9,10]
    3.   10,20,3             [10,20,3]    
    4.   41,20               [41,20]
    5.   1,5,3,4,2           [1,5,3,4,2]
    6.   1,0,0,0             [1,0,0,0]
    7.   0,0                 [0,0]
    8.   10,2,4,1,3,4,8,9,5  [10,2,4,1,3,4,8,9,5]
    9.   1,4,18,2,4,6        [1,4,18,2,4,6]
    10.  14,10,20,12         [14,10,20,12]

-}