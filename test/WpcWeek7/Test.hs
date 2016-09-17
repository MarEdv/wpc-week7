{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module WpcWeek7.Test
    (bplusSuite)
where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit
--import Test.Tasty.SmallCheck as SC

import WpcWeek7

-- Make instance of CoconutDataStruct
-- we simply use consN Constr where N is the arity of Constr (SmallCheck)
-- we also needed the
-- {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
--import Test.SmallCheck.Series

--instance Monad m => Serial m CoconutDataStruct  where series = cons1 CoconutConstr
-- Now we could test properties with smallcheck on CoconutDataStruct type.

bplusSuite :: TestTree
bplusSuite = testGroup "bplus"
    [ testCase "bplus" testInsert
    , testCase "searchNode" testSearchNode
    , testCase "insert" testInsert2
    , testCase "link" testLink
--    , SC.testProperty "bplus property" prop_bplus
    ]

testInsert :: Assertion
testInsert = assertEqual "Stuff" (Leaf 3 [(1,'a')] Nothing) (Leaf 3 [(1,'a')] Nothing)

bnode1 :: Tree Int Char
bnode1 = Leaf 3 [(6,'f'),(7,'g')] Nothing

btree1 :: Tree Int Char
btree1 = x where
  y3 = Leaf 3 [(6,'f'),(7,'g')] Nothing
  y2 = Leaf 3 [(3,'c'),(4,'d')] (Just y3)
  y1 = Leaf 3 [(1,'a'),(2,'b')] (Just y2)
  x = Node 3 [3,6] [y1, y2, y3] Nothing

btree2 :: Tree Int Char
btree2 = x where
  y4 = Leaf 3 [(6,'f'),(7,'g')] Nothing
  y3 = Leaf 3 [(5,'d')] (Just y4)
  y2 = Leaf 3 [(3,'c'),(4,'d')] (Just y3)
  y1 = Leaf 3 [(1,'a'),(2,'b')] (Just y2)
  x = Node 3 [3,5,6] [y1, y2, y3, y4] Nothing
  
testSearchNode :: Assertion
testSearchNode = assertEqual "yo" (Just bnode1) (searchNode 7 btree1)

testInsert2 :: Assertion
testInsert2 = assertEqual "yo2" btree2 (insert 5 'd' btree1)

testLink :: Assertion
testLink = assertEqual "link" [(Leaf 3 [(1,'a')] (Just (Leaf 3 [(2,'b')] Nothing))),(Leaf 3 [(2,'b')] Nothing)] (link [(Leaf 3 [(1,'a')] Nothing), (Leaf 3 [(2,'b')] Nothing)])

--prop_coconut :: Property IO
--prop_coconut = forAll $ \coconutStruct -> coconutfunc coconutStruct >= 0
