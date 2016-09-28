module WpcWeek7(BplusTree (..),searchNode,insert,link,findChild,createEmptyRoot) where
import Data.Maybe
import Data.List(sortOn)


{-
Data structure for a B+Tree according to the algorithm described at https://en.wikipedia.org/wiki/B%2B_tree

A BplusTree is parameterized with two types, one for keys and one for values.

A Node is a non-leaf node in the tree
 - arity is the maximum number of keys per node
 - keys is a list of the lowest key of each child (except the first child)
 - children is a list of children, either of type Node or Leaf. The list of children always contains one more element than the list of keys.
 - sibling is "the next Node node"
A Leaf is a leaf node in the tree
 - arity is the maximum number of keys per node
 - keyValues is a list of tuples of key and value
 - sibling is "the next Leaf node"
-}
data BplusTree key value =
    Node {
      arity :: Int
      , keys :: [key]
      , children :: [BplusTree key value]
      , sibling :: (Maybe (BplusTree key value))
    }
  |
    Leaf {
       arity :: Int
       , keyValues :: [(key, value)]
       , sibling :: (Maybe (BplusTree key value))
    }
  deriving (Eq)

showBplusTree :: (Show key, Show value) => BplusTree key value -> ShowS
showBplusTree (Leaf arity' keyValues' _) = shows ("Leafs " ++ (show arity') ++ " " ++ (show keyValues'))
showBplusTree (Node arity' keys' children' _) = shows ("Node " ++ (show arity') ++ " " ++ (show keys') ++ " " ++ (show children'))

instance (Show key, Show value) => Show (BplusTree key value) where
  showsPrec _ x = showBplusTree x

{-
Creates an empty root node with a specified arity.
-}
createEmptyRoot :: Int -> BplusTree key value
createEmptyRoot n = Node n [] [] Nothing

{-
Maybe finds a Leaf node that contains a certain key.
-}
searchNode :: (Ord key) => key -> BplusTree key value -> Maybe (BplusTree key value)
searchNode _ t@(Leaf _ _ _) = Just t
searchNode k (Node _ keys' children' _) = x where
  y = findChild k keys' children'
  x = case y of
    Just c -> searchNode k c
    Nothing -> Nothing

{-
A utility function of a flatmap
-}
flatmap :: (a -> [b]) -> [a] -> [b]
flatmap _ [] = []
flatmap f xs = (>>=) xs f
--flatmap f (x:xs) = f x ++ flatmap f xs

{-
The elements of the list are ascendingly ordered.
The function locates the first element that is higher than the key.
The BplusTree node in the same position contains the key.
-}
findChild :: (Ord key) => key -> [key] -> [BplusTree key value] -> Maybe (BplusTree key value)
findChild _ [] (c:[]) = Just c
findChild key (k:ks) (c:cs)
  | key < k = Just c
  | otherwise = findChild key ks cs
findChild _ _ _ = Nothing

{-
Inserts a key mapping to a value into a BplusTree. The function returns the new root
node of a new tree in line with other persistent data structures.
-}
insert :: (Ord key, Eq value) => key -> value -> BplusTree key value -> BplusTree key value
insert key' value' root@(Node arity' _ _ _) = x where
  ys = insert2 key' value' root
  x = if (length ys == 1)
      then head ys
      else createNode arity' Nothing ys

insert _ _ root = root

{-
A helper function that inserts a key-value pair and returns a list of tree nodes.
The function returns two elements in the list when a node has been split into
two nodes, to be able to honor the tree's arity.
-}
insert2 :: (Ord key, Eq value) => key -> value -> BplusTree key value -> [BplusTree key value]
insert2 key' value' (Node arity' keys' children' _) = x where
  Just child = findChild key' keys' children'
  newChildren = flatmap (\c -> if (child == c)
                     then (insert2 key' value' c)
                     else [c]
              )
              children'
  (n1, n2) = perhapsSplitArray arity' newChildren
  newNode = if (length n2 > 0)
            then [createNode arity' Nothing n2]
            else []
  x = [createNode arity' Nothing n1] ++ newNode
insert2 key' value' (Leaf arity' keyValues' _) = x where
  kvsExceptNewKey = filter (\(d,_) -> d /= key') keyValues'
  kvsWithNewKey = kvsExceptNewKey ++ [(key',value')]
  sortedKeyValues = sortOn (\(a,_) -> a) kvsWithNewKey
  (kv1, kv2) = perhapsSplitArray arity' sortedKeyValues
  newLeaf = if (length sortedKeyValues < arity')
            then []
            else [Leaf arity' kv2 Nothing]
  x = [Leaf arity' kv1 Nothing] ++ newLeaf

createNode :: (Ord key) => Int -> Maybe (BplusTree key value) -> [BplusTree key value] -> BplusTree key value
createNode arity' sibling' children' = node where
  childIndex = flatmap getLowestKey children'
  newChildIndex = if (length childIndex > 0)
                  then tail childIndex
                  else []
  linkedChildren = link children'
  node = Node arity' newChildIndex linkedChildren sibling'

{-
Gets the lowest key of the Leaf or of the Leaf reachable from a Node.
-}
getLowestKey :: BplusTree key value -> [key]
getLowestKey c = case c of
  Leaf _ kv _ -> [fst (head kv)]
  Node _ _ cs _ -> getLowestKey (head cs)

{-
If the element count in the array is equal to larger than the numeric argument,
the array is split into two as equally size arrays as possible.
  -}
perhapsSplitArray :: Int -> [a] -> ([a], [a])
perhapsSplitArray n xs = (kv1, kv2) where
  halfN = n `div` 2 + 1
  (kv1, kv2) = if (length xs < n)
               then (xs,[])
               else (take halfN xs, drop halfN xs)

{-
Updates the sibling field of all Leaf or Node objects in the list
passed as first argument. Each element will reference to the next in the list
as its sibling.
-}
link :: (Eq keys) => [BplusTree keys values] -> [BplusTree keys values]
link [] = []
link (x:xs) = z where
  y = link $ xs
  z = case x of
    Leaf arity' keys' _ -> [(Leaf arity' keys' (listToMaybe y))] ++ y
    Node arity' keys' children' _ -> [(Node arity' keys' children' (listToMaybe y))] ++ y
