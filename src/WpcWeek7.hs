module WpcWeek7(Tree (..),searchNode,insert,link) where
import Data.Maybe
import Data.List(sortOn)

data Tree key value =
  Node Int [key] [Tree key value] (Maybe (Tree key value))
  |
  Leaf Int [(key, value)] (Maybe (Tree key value))
  deriving (Show, Eq)

searchNode :: (Eq key) => key -> Tree key value -> Maybe (Tree key value)
searchNode _ t@(Leaf _ _ _) = Just t
searchNode k (Node _ keys children _) = x where
  y = findChild k keys children
  x = case y of
    Just c -> searchNode k c
    Nothing -> Nothing

findChild :: (Eq a) => a -> [a] -> [b] -> Maybe b
findChild key (k:[]) (c:cs)
  | key == k = Just c
  | otherwise = Just (head cs)
findChild key (k:ks) (c:cs)
  | key == k = Just c
  | otherwise = findChild key ks cs
findChild _ _ _ = Nothing

flatmap _ [] = []
flatmap f (x:xs) = f x ++ flatmap f xs

findChild2 :: (Ord a) => a -> [a] -> [Tree key value] -> Tree key value
findChild2 _ [] (c:[]) = c
findChild2 key (k:ks) (c:cs)
  | key < k = c
  | otherwise = findChild2 key ks cs

insert :: (Ord key, Eq value) => key -> value -> Tree key value -> Tree key value
insert k v n = head $ insert2 k v n

insert2 :: (Ord key, Eq value) => key -> value -> Tree key value -> [Tree key value]
insert2 k v n@(Node arity keys children sibling) = x where
  a = findChild2 k keys children
  y = flatmap (\c -> if (a == c) then (insert2 k v c) else [c]) children
  z = map (\c -> case c of
              Leaf _ kv _ -> fst (head kv)
              Node _ ks _ _ -> head $ ks
          ) y
  x = [Node arity (tail z) (link y) sibling]
insert2 k v l@(Leaf arity keyValues sibling) = x where
  z = keyValues ++ [(k,v)]
  y = sortOn (\(a,_) -> a) z
  halfArity = arity `div` 2 + 1
  (kv1, kv2) = if (length y < arity)
               then (y,[])
               else (take halfArity y, drop halfArity y)
  c1 = if (length y < 3)
       then []
       else [Leaf arity kv2 Nothing]
  x = [Leaf arity kv1 Nothing] ++ c1

link :: (Eq keys) => [Tree keys values] -> [Tree keys values]
link [] = []
link (x:xs) = z where
  y = link $ xs
  z = case x of
    Leaf a keys k -> [(Leaf a keys (listToMaybe y))] ++ y
    Node _ _ _ _ -> []
