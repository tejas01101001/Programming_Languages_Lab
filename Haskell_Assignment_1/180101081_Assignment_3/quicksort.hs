{-
    Author : Tejas Khairnar
    Roll number : 180101081

    1. The code was written ,complied and executed in Ubuntu 20.04 using
       Glasgow Haskell Compiler, Version 8.6.5, stage 2 booted by GHC version 8.6.5
    2. Make sure you have ghc installed on your system
    3. Change the working directory to the source code
    4. To compile the code and create the object file type the following command in terminal
      'ghc -o obj quicksort.hs'
    5. To run the code type the following command in terminal
      './obj'
    6. 5 testcases are hardcoded in the main function 
    7. To give a custom input 
       > First start the interpreter by typing 'ghci' in terminal
       > Load the source code by typing ' :l quicksort.hs '
       > Given the custom input by typing 'quickSort [5,4,80,1]' the array can be changed
    8. The worst case time complexity of this particular code is O(n^2) ans the expected case time complexity is O(nlogn)

-}

-- Function for quick sort
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
-- Parition the array into two parts using the pivot element and recursively call the function for both the halves
quickSort (x : xs) = quickSort leftArray ++ (x : quickSort rightArray)
  where
    leftArray = [e | e <- xs, e <= x] -- Create a new array with values less than or equal to the pivot
    rightArray = [e | e <- xs, e > x] -- Create a new array with values greater than the pivot


-- Main Function
main :: IO ()
main = do
  -- Test Case 1
  putStrLn "Test Case 1:"
  putStrLn "The Input Array is [8,2,1,90]"
  putStr "The Sorted Array " 
  print(quickSort [8,2,1,90])

  --Test Case 2
  putStrLn "Test Case 2:"
  putStrLn "The Input Array is [18,12,11,9]"
  putStr "The Sorted Array " 
  print(quickSort [18,12,11,9])


  --Test Case 3
  putStrLn "Test Case 3:"
  putStrLn "The Input Array is [1,8,11,9,100,1,2,222]"
  putStr "The Sorted Array " 
  print(quickSort [1,8,11,9,100,1,2,222])


  --Test Case 4
  putStrLn "Test Case 4:"
  putStrLn "The Input Array is [1,80,1]"
  putStr "The Sorted Array " 
  print(quickSort [1,80,1])


  --Test Case 5
  putStrLn "Test Case 5:"
  putStrLn "The Input Array is [12, 2, 4, 5, 18]"
  putStr "The Sorted Array " 
  print(quickSort [12, 2, 4, 5, 18])


  