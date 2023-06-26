import State (State (..))

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

buildTree :: [a] -> Tree a
buildTree [] = error "Empty list"
buildTree [x] = Leaf x
buildTree xs = Node (buildTree firstHalf) (buildTree secondHalf)
  where
    (firstHalf, secondHalf) = splitAt ((length xs + 1) `div` 2) xs

numberOfLeaves :: Tree a -> Int
numberOfLeaves (Leaf _) = 1
numberOfLeaves (Node l r) = numberOfLeaves l + numberOfLeaves r

-- Without State Monad
relabel :: Tree a -> Int -> (Tree (Int, a), Int)
relabel (Leaf x) i = (Leaf (i, x), i + 1)
relabel (Node l r) i =
  let (lTree, counter) = relabel l i
      (rTree, counter') = relabel r counter
   in (Node lTree rTree, counter')

-- With State Monad
relabel' :: Tree a -> State Int (Tree (Int, a))
relabel' (Leaf x) = State $ \i -> (Leaf (i, x), i + 1)
relabel' (Node l r) = do
  a <- relabel' l
  b <- relabel' r
  return (Node a b)
