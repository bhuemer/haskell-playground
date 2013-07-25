
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

data Path a = L a (Tree a) | R a (Tree a) deriving (Show)

splay :: Tree a -> [Path a] -> Tree a
splay tree [] = tree

-- Zig cases
splay (Node x a b) [L p c] = Node x a (Node p b c)
splay (Node x b a) [R p c] = Node x (Node p c b) a

-- Zig Zig cases
splay (Node x a b) ((L gp d) : (L p c) : path) = splay (Node x a (Node p b (Node gp c d))) path
splay (Node x b a) ((R gp d) : (R p c) : path) = splay (Node x (Node p (Node gp d c) b) a) path

-- Zig Zag cases
splay (Node x b c) ((L gp d) : (R p a) : path) = splay (Node x (Node p a b) (Node gp c d)) path 
splay (Node x c b) ((R gp d) : (L p a) : path) = splay (Node x (Node gp d c) (Node p b a)) path 

insert :: (Ord a) => Tree a -> a -> Tree a
insert tree a = insert' tree a [] 
    where insert' :: (Ord a) => Tree a -> a -> [Path a] -> Tree a
          insert' Empty a path = splay (Node a Empty Empty) path
          insert' (Node x l r) a path 
            | x < a     = insert' r a ((R x l) : path)
            | otherwise = insert' l a ((L x r) : path)

find :: (Ord a) => Tree a -> a -> Tree a
find tree a = find' tree a []
    where find' :: (Ord a) => Tree a -> a -> [Path a] -> Tree a
          find' (Node x l Empty) a path | x < a = splay (Node x l Empty) path
          find' (Node x l r) a path     | x < a = find' r a ((R x l) : path)
          find' (Node x Empty r) a path = splay (Node x Empty r) path
          find' (Node x l r) a path     
              | x > a = find' l a ((L x r) : path)
              | otherwise = splay (Node x l r) path