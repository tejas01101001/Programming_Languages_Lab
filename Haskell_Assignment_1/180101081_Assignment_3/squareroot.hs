{-
    Author : Tejas Khairnar
    Roll number : 180101081

    1. The code was written ,complied and executed in Ubuntu 20.04 using
       Glasgow Haskell Compiler, Version 8.6.5, stage 2 booted by GHC version 8.6.5
    2. Make sure you have ghc installed on your system
    3. Change the working directory to the source code
    4. To compile the code and create the object file type the following command in terminal
      'ghc -o obj squareroot.hs'
    5. To run the code type the following command in terminal
      './obj'
    6. Enter the number whose square root you want to find.
    7. I have also given 10 testcases at the end of the code for which you can test the code
    8. The time complexity of this particular code is O(log n) due to the binary search function
    
-}

-- Function to find the rounded value correct upto 5 decimal places
fiveDecimals :: Double -> Double
fiveDecimals x = fromIntegral (round $ x * 1e5) / 1e5

-- Binary Search function to find the square root of a given number
findSquareRoot :: (Ord t, Fractional t) => t -> t -> t -> t
findSquareRoot value low high
  | high - low < 0.000001 = high -- If the value of low and high is within the desired tolerance range return
  | mid * mid < value = findSquareRoot value mid high -- If the current number's square is less than the actual value search the right half
  | mid * mid >= value = findSquareRoot value low mid -- If the current number's square is atleast the actual value search the left half
  where
    mid = low + ((high - low) / 2) -- Value of mid value i.e the average of high and low

-- Main function
main :: IO ()
main = do
  -- Read the number whose square root is to be found
  putStrLn "Enter the number"
  num <- readLn

  -- Calculate the square root of the given number
  let low = 0
  let high = max 1 num
  let number = findSquareRoot num low high

  -- Print the square root of the given number correct upto 5 decimal places
  putStr "The square root of the given number is "
  print (fiveDecimals (number))

------------ TESTCASES ----------------
{-
         Input             Output
    1.   23.56             4.85386
    2.   100000000000      316227.76602
    3.   7                 2.64575
    4.   81                9.0
    5.   120               10.95445
    6.   17382131248       131841.31086
    7.   441               21.0
    8.   100               10.0
    9.   0.6673            0.81688
    10.  0.27173           0.52128

-}