module HW1 where
  -- Joshua Strozzi
  
  -- | Integer-labeled binary trees.
  data Tree
     = Node Int Tree Tree   -- ^ Internal nodes
     | Leaf Int             -- ^ Leaf nodes
    deriving (Eq,Show)
  
  
  -- | An example binary tree, which will be used in tests.
  t1 :: Tree
  t1 = Node 1 (Node 2 (Node 3 (Leaf 4) (Leaf 5))
                      (Leaf 6))
              (Node 7 (Leaf 8) (Leaf 9))
  
  -- | Another example binary tree, used in tests.
  t2 :: Tree
  t2 = Node 6 (Node 2 (Leaf 1) (Node 4 (Leaf 3) (Leaf 5)))
              (Node 8 (Leaf 7) (Leaf 9))
  
  
  -- | The integer at the left-most node of a binary tree.
  --
  --   >>> leftmost (Leaf 3)
  --   3
  --
  --   >>> leftmost (Node 5 (Leaf 6) (Leaf 7))
  --   6
  --
  --   >>> leftmost t1
  --   4
  --
  --   >>> leftmost t2
  --   1
  --
  -- as I understand it: if the argument is a leaf with a value, it returns the leafs value
  -- if it's a node, it ignors the value of the node and the right tree of the node and calls itself with
  -- the pararmeter of the left child node
  leftmost:: Tree -> Int
  leftmost (Leaf i) = i
  leftmost (Node _ l _) =  leftmost l
  
  
  -- | The integer at the right-most node of a binary tree.
  --
  --   >>> rightmost (Leaf 3)
  --   3
  --
  --   >>> rightmost (Node 5 (Leaf 6) (Leaf 7))
  --   7
  --
  --   >>> rightmost t1
  --   9
  --
  --   >>> rightmost t2
  --   9
  --
  -- Does the same as the leftmost, but descends into the right node instead of the left nodes
  rightmost:: Tree->Int
  rightmost (Leaf i) = i
  rightmost (Node _ _ l) = rightmost (l)
  
  
  -- | Get the maximum integer from a binary tree.
  --
  --   >>> maxInt (Leaf 3)
  --   3
  --
  --   >>> maxInt (Node 5 (Leaf 4) (Leaf 2))
  --   5
  --
  --   >>> maxInt (Node 5 (Leaf 7) (Leaf 2))
  --   7
  --
  --   >>> maxInt t1
  --   9
  --
  --   >>> maxInt t2
  --   9
  --
  -- If leaf, returns value
  -- it descends into the leftmost node and compares the values of the left leaf and right leaf, and current nodes value
  -- then acsends to the node above that and compares the maximum of the last comparison to the current node,
  -- and then would descend into the right node to get it's max value
  
  -- Hopefully that makes sense
  maxInt :: Tree -> Int
  maxInt (Leaf i) = i
  maxInt (Node n l r) = maximum[n, maxInt l, maxInt r]
  
  
  -- | Get the minimum integer from a binary tree.
  --
  --   >>> minInt (Leaf 3)
  --   3
  --
  --   >>> minInt (Node 2 (Leaf 5) (Leaf 4))
  --   2
  --
  --   >>> minInt (Node 5 (Leaf 4) (Leaf 7))
  --   4
  --
  --   >>> minInt t1
  --   1
  --
  --   >>> minInt t2
  --   1
  --
  -- Same as maxInt, but instead compares using the minimum function, instead of maximum
  minInt:: Tree -> Int
  minInt (Leaf i) = i
  minInt (Node n l r) = minimum[n, minInt l, minInt r]
  
  
  -- | Get the sum of the integers in a binary tree.
  --
  --   >>> sumInts (Leaf 3)
  --   3
  --
  --   >>> sumInts (Node 2 (Leaf 5) (Leaf 4))
  --   11
  --
  --   >>> sumInts t1
  --   45
  --
  --   >>> sumInts (Node 10 t1 t2)
  --   100
  --
  -- Like all my functions, uses pattern matching
  -- If argument is leaf, grab it's value
  -- if node, add current node value to recursive call of sumInts descending into both left and right Nodes
  sumInts               :: Tree -> Int
  --sumInts (Node 0 _ _)  = 0 --Fail
  --sumInts (Leaf 0) = 0 -- fail
  sumInts (Leaf a)      = a
  sumInts (Node n l r)  = n + sumInts l + sumInts r
  
  
  -- | The list of integers encountered by a pre-order traversal of the tree.
  --
  --   >>> preorder (Leaf 3)
  --   [3]
  --
  --   >>> preorder (Node 5 (Leaf 6) (Leaf 7))
  --   [5,6,7]
  --
  --   >>> preorder t1
  --   [1,2,3,4,5,6,7,8,9]
  --
  --   >>> preorder t2
  --   [6,2,1,4,3,5,8,7,9]
  --
  -- if it's currently on a leaf or a node, it adds it to the array, then (if node) will go down the left
  -- to grab the next value, and then grab the right node or leaf value 
  preorder              :: Tree -> [Int]
  preorder (Leaf a)     = [a]
  preorder (Node n l r) = n : preorder l ++ preorder r
  --preorder (Node n l r) =[n]: preorder l : preorder r     --nope
  
  
  -- | The list of integers encountered by an in-order traversal of the tree.
  --
  --   >>> inorder (Leaf 3)
  --   [3]
  --
  --   >>> inorder (Node 5 (Leaf 6) (Leaf 7))
  --   [6,5,7]
  --
  --   >>> inorder t1
  --   [4,3,5,2,6,1,8,7,9]
  --
  --   >>> inorder t2
  --   [1,2,3,4,5,6,7,8,9]
  --
  -- grabs left most leaf then the value of the  node above it, then that nodes
  -- right value, and so on
  inorder               :: Tree -> [Int]
  inorder (Leaf a)      = [a]
  inorder (Node n l r)  = inorder l ++ (n : inorder r)
  --inorder (Node n l r)  = (inorder l) : n ++ inorder r  --nope
  
  
  
  
  ----------helper function---------------
  
  --This is a helper function that returns the value of a node or leaf, used in my
  -- isBST function that way I don't have to do as much work in isBST
  getVal::Tree -> Int
  getVal (Leaf a) = a
  getVal (Node v l r) = v
  
  -- | Check whether a binary tree is a binary search tree.
  --
  --   >>> isBST (Leaf 3)
  --   True
  --
  --   >>> isBST (Node 5 (Leaf 6) (Leaf 7))
  --   False
  --
  --   >>> isBST t1
  --   False
  --
  --   >>> isBST t2
  --   True
  --
  -- Help: http://learnyouahaskell.com/syntax-in-functions#guards-guards 
  --isBST :: Tree -> Int -> Bool ---nop
  isBST :: Tree -> Bool
  --isBST (Leaf a ) = a --Nope
  isBST (Leaf _) = True
  -- isBST (Node v l r) = (v > isBST l) && (v < isBST r) --nope
  isBST (Node v l r)
    | getVal l > v = False
    | getVal r < v = False
    | otherwise = True
  
  -- | Check whether a number is contained in a binary search tree.
  --   (You may assume that the given tree is a binary search tree.)
  --
  --   >>> inBST 2 (Node 5 (Leaf 2) (Leaf 7))
  --   True
  --
  --   >>> inBST 3 (Node 5 (Leaf 2) (Leaf 7))
  --   False
  --
  --   >>> inBST 4 t2
  --   True
  --
  --   >>> inBST 10 t2
  --   False
  --
  --isBST Int -> Bool         --failed
  inBST :: Int->Tree->Bool     -- takes an int and a tree and turns it into a bool
  inBST a (Leaf i) 
    | a == i = True
    | otherwise = False
  inBST a (Node v l r)
    | a == v = True
    | otherwise = (inBST a l) || (inBST a r)
    --  | otherwise = ((a == getVal(l)) || (a == getVal(r)))  --failed
  
  --inBST a (Tree t)                --failed
  --  | a == getVal t = True        --failed
  --  | a \= getVal t = False       --failed
    
  --  | a == getVal(l) = True     --failed
  --  | a == getVal(r) = True     --failed
  --  | otherwise = False         --failed
  
  