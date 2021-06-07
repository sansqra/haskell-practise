-- Tree Type

data Tree a = EmpTree | Node a (Tree a) (Tree a) deriving (Read, Eq, Show)


-- construct tree of single node

singleton :: a -> Tree a
singleton x = Node x EmpTree EmpTree

-- Insertion

insert :: (Ord a) => a -> Tree a -> Tree a
insert x EmpTree = singleton x
insert x (Node a left right)
  | x == a = (Node a left right)
  | x < a = Node a (insert x left) right
  | x > a = Node a left (insert x right)


-- if element is in tree

inTree :: (Ord a) => a -> Tree a -> Bool
inTree x EmpTree = False
inTree x (Node a left right)
  | x == a = True
  | x < a = inTree x left
  | x > a = inTree x right
