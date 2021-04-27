{-
    Author : Tejas Khairnar
    Roll number : 180101081

    1. The code was written ,complied and executed in Ubuntu 20.04 using
       Glasgow Haskell Compiler, Version 8.6.5, stage 2 booted by GHC version 8.6.5
    2. Make sure you have ghc installed on your system
    3. Change the working directory to the source code
    4. To compile the code and create the object file type the following command in terminal
      'ghc -o obj bst.hs'
    5. To run the code type the following command in terminal
      './obj'
    6. Enter the comma seperated input list (without square brackets)
    7. I have also given 5 testcases at the end of the code for which you can test the code
    8. The BST is created in the order of the given input

-}

import Data.List ()
import Data.List.Split (splitOn)
import System.IO ()

-- Declaration of data Tree
data Tree a = Null | Leaf a | Node (Tree a) a (Tree a) deriving (Show)

-- Creation of new node in the tree
newNode :: a -> Tree a
newNode = Leaf

-- Insert a node with a given key into the tree
insertNode :: (Ord a) => Tree a -> a -> Tree a
-- If the tree is null initially
insertNode Null key = newNode key
-- If the tree has only the leaf node
insertNode (Leaf x) key
  | key == x = Node Null x Null -- BST has unique keys
  | key < x = Node (newNode key) x Null -- If key is less than the current node value insert on left side
  | otherwise = Node Null x (newNode key) -- If key is more than the current node value insert on right side

-- If the tree has both non Null left and right pointers
insertNode (Node l x r) key
  | key == x = Node l x r -- BST has unique keys
  | key < x = Node (insertNode l key) x r -- If key is less than the current node value insert on left side
  | otherwise = Node l x (insertNode r key) -- If key is more than the current node value insert on right side

-- Creation of the Binary search tree from a given list
makeBST :: Ord a => [a] -> Tree a
makeBST [] = Null -- Empty list results in a empty tree
makeBST (x : xs) = create (Node Null x Null) xs -- First node of the list as the root
  where
    create tree [] = tree -- Empty list results into the current tree itself
    create tree (x : xs) = create (insertNode tree x) xs -- Insert the head of the list and reccurse for the remaining list

-- Preorder Traversal of the binary search tree
preorder :: Tree a -> [a]
preorder Null = []
preorder (Leaf x) = [x]
preorder (Node l x r) = [x] ++ preorder l ++ preorder r

-- Inorder Traversal of the binary search tree
inorder :: Tree a -> [a]
inorder Null = []
inorder (Leaf x) = [x]
inorder (Node l x r) = inorder l ++ [x] ++ inorder r

-- Postorder Traversal of the binary search tree
postorder :: Tree a -> [a]
postorder Null = []
postorder (Leaf x) = [x]
postorder (Node l x r) = postorder l ++ postorder r ++ [x]

main :: IO ()
main = do
  putStrLn "Enter the comma seperated input list (without square brackets)"
  
  -- Read the comma seperated input and convert it into a list
  s <- getLine
  let list = map (read :: String -> Int) (splitOn "," s)

  -- Create the Binary search Tree using the list 
  let tree = makeBST list

  -- Preorder Travesal of the Tree
  let preorderTraversal = preorder tree
  putStr "The preorder traversal of the given Tree is "
  print (preorderTraversal)

  -- Inorder Travesal of the Tree
  let inorderTraversal = inorder tree
  putStr "The inorder traversal of the given Tree is "
  print (inorderTraversal)

  -- Postorder Travesal of the Tree
  let postorderTraversal = postorder tree
  putStr "The postorder traversal of the given Tree is "
  print (postorderTraversal)

------------ TESTCASES ----------------
{-

Input 1 :
4,5,2,3,1

Output 1:

The preorder traversal of the given Tree is [4,2,1,3,5]
The inorder traversal of the given Tree is [1,2,3,4,5]
The postorder traversal of the given Tree is [1,3,2,5,4]

Actual Binary Search Tree formed 1:

      4
     / \
    2   5
   / \
  1   3

---------------------------------------------------------------

Input 2 :
100

Output 2:

The preorder traversal of the given Tree is [100]
The inorder traversal of the given Tree is [100]
The postorder traversal of the given Tree is [100]

Actual Binary Search Tree formed 2:

    100

--------------------------------------------------------------

Input 3 :
1,3,2

Ouput 3:

The preorder traversal of the given Tree is [1,3,2]
The inorder traversal of the given Tree is [1,2,3]
The postorder traversal of the given Tree is [2,3,1]

Actual Binary Search Tree formed 3:

        1
         \
          3
           \
            2

--------------------------------------------------------------

Input 4 :
2,3,1

Ouput 4:

The preorder traversal of the given Tree is [2,1,3]
The inorder traversal of the given Tree is [1,2,3]
The postorder traversal of the given Tree is [1,3,2]

Actual Binary Search Tree formed 4:

        2
       / \
      1   3

--------------------------------------------------------------

Input 5 :
2,6,7,3,1,5,4

Ouput 5:

The preorder traversal of the given Tree is [2,1,6,3,5,4,7]
The inorder traversal of the given Tree is [1,2,3,4,5,6,7]
The postorder traversal of the given Tree is [1,4,5,3,7,6,2]

Actual Binary Search Tree formed 5:

        2
       / \
      1   6
         / \
        3   7
         \
          5
         /
        4

--------------------------------------------------------------

-}