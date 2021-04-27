{-
    Author : Tejas Khairnar
    Roll number : 180101081

    1. The code was written ,complied and executed in Ubuntu 20.04 using
       Glasgow Haskell Compiler, Version 8.6.5, stage 2 booted by GHC version 8.6.5
    2. Make sure you have ghc installed on your system
    3. Change the working directory to the source code
    4. To compile the code and create the object file type the following command in terminal
      'ghc -o obj lcm.hs'
    5. To run the code type the following command in terminal
      './obj'
    6. Enter the comma seperated input list (without square brackets)
    7. I have also given 10 testcases at the end of the code for which you can test the code
    8. The time complexity of this particular code is O(n) due to the list traversal

-}

import Data.List ()
import Data.List.Split (splitOn)
import System.IO ()

-- Custom function to find gcd of two numbers (euclid's algorithm)
_gcd :: Integral t => t -> t -> t
_gcd a b
  | b == 0 = a
  | otherwise = _gcd b (a `mod` b)

-- Custom function to find lcm of two numbers
_lcm :: Integral p => p -> p -> p
_lcm _ 0 = 0 -- lcm of 0 with any non negative number is 0
_lcm 0 _ = 0 -- lcm of 0 with any non negative number is 0
_lcm x y = abs (x `quot` _gcd x y) * y -- lcm * gcd = product of two numbers

-- Recursive function to calculate the LCM of the list , O(n) function
listLcm :: Integral p => [p] -> p
listLcm [] = 1
listLcm (x : xs) = _lcm x (listLcm xs)

main :: IO ()
main = do
  putStrLn "Enter the comma seperated input list (without square brackets)"

  -- Read the comma seperated input and convert it into a list
  s <- getLine
  let list = map (read :: String -> Int) (splitOn "," s)

  -- Find the LCM of the given list
  let ans = listLcm list
  putStr "The LCM i.e Least common multiple of the given list is "
  print ans

------------ TESTCASES ----------------
{-
         Input              Output
    1.   192                 192
    2.   1,9,10              90
    3.   10,20,3             60
    4.   41,20               820
    5.   1,5,3,4,2           60
    6.   1,0,0,0             0
    7.   0,0                 0
    8.   10,2,4,1,3,4,8,9,5  360
    9.   1,4,18,2,4,6        36
    10.  14,10,20,12         420

-}