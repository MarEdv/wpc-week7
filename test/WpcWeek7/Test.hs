{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module WpcWeek7.Test
    (bplusSuite,bulkInsert,containsAll)
where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC
import Data.List (nub)

import WpcWeek7

bplusSuite :: TestTree
bplusSuite = testGroup "bplus"
    [ testCase "bplus" testInsert
    , testCase "searchNode" testSearchNode
    , testCase "insert" testInsert2
    , testCase "linkLeafs" testLinkLeafs
    , testCase "linkNodes" testLinkNodes
    , testCase "maxArity" testMaxArity
    , SC.testProperty "insertedElementIsFound" insertedElementIsFound
    , SC.testProperty "headHasCorrectNoChildren" headHasCorrectNoChildren
    , SC.testProperty "headHasCorrectNoChildren2" headHasCorrectNoChildren2
    ]

testInsert :: Assertion
testInsert = assertEqual "Stuff" ((Leaf 3 [(1,'a')] Nothing) :: (BplusTree Int Char)) (Leaf 3 [(1,'a')] Nothing)

bnode1 :: BplusTree Int Char
bnode1 = Leaf 3 [(6,'f'),(7,'g')] Nothing

btree1 :: BplusTree Int Char
btree1 = x where
  y3 = Leaf 3 [(6,'f'),(7,'g')] Nothing
  y2 = Leaf 3 [(3,'c'),(4,'d')] (Just y3)
  y1 = Leaf 3 [(1,'a'),(2,'b')] (Just y2)
  x = Node 3 [3,6] [y1, y2, y3] Nothing

btree2 :: BplusTree Int Char
btree2 = x where
  y4 = Leaf 3 [(6,'f'),(7,'g')] Nothing
  y3 = Leaf 3 [(5,'d')] (Just y4)
  y2 = Leaf 3 [(3,'c'),(4,'d')] Nothing
  y1 = Leaf 3 [(1,'a'),(2,'b')] (Just y2)
  z2 = Node 3 [6] [y3, y4] Nothing
  z1 = Node 3 [3] [y1, y2] (Just z2)
  x = Node 3 [5] [z1, z2] Nothing

testSearchNode :: Assertion
testSearchNode = assertEqual "yo" (Just bnode1) (searchNode 7 btree1)

testInsert2 :: Assertion
testInsert2 = assertEqual "yo2" btree2 (insert 5 'd' btree1)

testLinkLeafs :: Assertion
testLinkLeafs = assertEqual "link" ([(Leaf 3 [(1,'a')] (Just (Leaf 3 [(2,'b')] Nothing))),(Leaf 3 [(2,'b')] Nothing)] :: [BplusTree Int Char]) (link [(Leaf 3 [(1,'a')] Nothing), (Leaf 3 [(2,'b')] Nothing)])

testLinkNodes :: Assertion
testLinkNodes = assertEqual "link" ([(Node 3 [] [] (Just (Node 3 [] [] Nothing))),(Node 3 [] [] Nothing)] :: [BplusTree Int Char]) (link [(Node 3 [] [] Nothing), (Node 3 [] [] Nothing)])

testMaxArity :: Assertion
testMaxArity = assertEqual "maxArity" 2 (maxArity $ Just btree1)

maxArity :: Maybe (BplusTree keys values) -> Int
maxArity Nothing = 0
maxArity (Just node) = x where
  y = case node of
    Leaf _ kv s -> (length kv, maxArity s)
    Node _ kv _ s -> (length kv, maxArity s)
  x = max (fst y) (snd y)

insertedElementIsFound :: Property IO
insertedElementIsFound = forAll $ \list -> containsAll (list::[Int]) (bulkInsert (list))

headHasCorrectNoChildren :: Property IO
headHasCorrectNoChildren = forAll $ \list -> length (nub list) < 3 || case (bulkInsert (list::[Int])) of
  Node arity' _ children' _ -> length children' >= 2 && length children' <= arity'
  _ -> False

headHasCorrectNoChildren2 :: Property IO
headHasCorrectNoChildren2 = forAll $ \list -> length (nub list) > 3 || case (bulkInsert (list::[Int])) of
  Node arity' _ children' _ -> length children' >= 1 && length children' < arity'
  _ -> False

containsAll :: [Int] -> BplusTree Int values -> Bool
containsAll (x:xs) node = case searchNode x node of
  Just (Leaf _ keyValues' _) -> (length $ filter (\a -> fst a == x) keyValues') == 1 && containsAll xs node
  _ -> False
containsAll [] _ = True

bulkInsert :: [Int] -> BplusTree Int String
bulkInsert [] = Node 3 [] [Leaf 3 [] Nothing] Nothing
bulkInsert (x:xs) = insert x (show x) (bulkInsert xs)

