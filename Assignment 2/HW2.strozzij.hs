module HW2 where

    -- | Binary trees with nodes labeled by values of an arbitrary type.
    data Tree a
       = Node a (Tree a) (Tree a)
       | End
      deriving (Eq,Show)
    
    -- | One step in a path, indicating whether to follow the left subtree (L)
    --   or the right subtree (R).
    data Step = L | R
      deriving (Eq,Show)
    
    -- | A path is a sequence of steps. Each node in a binary tree can be
    --   identified by a path, indicating how to move down the tree starting
    --   from the root.
    type Path = [Step]
    
    -- | Create a leaf node.
    leaf :: a -> Tree a
    leaf x = Node x End End
    
    -- | An example tree.
    ex :: Tree Int
    ex = Node 4 (Node 3 (leaf 2) End)
                (Node 7 (Node 5 End (leaf 6))
                        (leaf 8))
    
    
    -- | Map a function over a tree. Applies the given function to every label
    --   in the tree, preserving the tree's structure.
    --   
    --   >>> mapTree odd End
    --   End
    --
    --   >>> mapTree even (Node 5 (leaf 2) End)
    --   Node False (Node True End End) End
    --
    --   >>> (mapTree not . mapTree even) (Node 5 End (leaf 2))
    --   Node True End (Node False End End)
    --
    --   >>> mapTree (+10) ex
    --   Node 14 (Node 13 (Node 12 End End) End) (Node 17 (Node 15 End (Node 16 End End)) (Node 18 End End))
    --
    --   >>> ex == (mapTree (subtract 27) . mapTree (+27)) ex
    --   True
    --
    mapTree :: (a -> b) -> Tree a -> Tree b
    mapTree f End = End
    mapTree f (Node v End End) = Node (f v) End End
    mapTree f (Node i l r) = Node (f i) (mapTree f l) (mapTree f r)
    
    
    -- | Get the value at the node specified by a path. Returns 'Nothing' if
    --   the given path is invalid.
    --
    --   >>> valueAt [] ex
    --   Just 4
    --
    --   >>> valueAt [L,L] ex
    --   Just 2
    --
    --   >>> valueAt [L,R] ex
    --   Nothing
    --
    --   >>> valueAt [R,L,R] ex
    --   Just 6
    --
    --   >>> valueAt [L,L,L] ex
    --   Nothing
    --
    valueAt :: Path -> Tree a -> Maybe a
    valueAt [] (Node v l r) = Just v -- if at node and list is empty then just value 
    valueAt [] End = Nothing         -- if at end of list and the node is End, then nothing
    valueAt (x:xs) (Node v l r)       --other wise if list and node, then 
      | x == L = valueAt xs l         -- if current list value is L Step, then call value at function for tail of list and left node
      | x == R = valueAt xs r         --again for right node

    
    
    -- | Find a path to a node that contains the given value.
    --
    --   >>> pathTo 3 (leaf 5)
    --   Nothing
    --
    --   >>> pathTo 5 ex
    --   Just [R,L]
    --
    --   >>> pathTo 6 ex
    --   Just [R,L,R]
    --
    --   >>> pathTo 4 ex
    --   Just []
    --
    --   >>> pathTo 10 ex
    --   Nothing
    --

    -- helper:
    pathHelper :: Step -> Maybe Path -> Maybe Path
    pathHelper _ Nothing = Nothing                  --
    pathHelper v (Just []) = (Just [v])             --If I'm where I want to go, then just spit out the step
    pathHelper v (Just (x:xs)) = Just ([v] ++ x:xs) --


    
    pathTo :: Eq a => a -> Tree a -> Maybe Path
    pathTo v (End) = Nothing
    pathTo v (Node i l r)  --base case
      | v == i = Just []      
      | left /= Nothing = left          --if a not == nothing then a
      | right /= Nothing = right         --if b not == nothing then b
      | otherwise = Nothing                -- else
        where left = pathHelper L (pathTo v l)      -- go left
              right = pathHelper R (pathTo v r)     -- go right


-- Aiden's friend didn't use Intersperse at all
-- Used intercalate