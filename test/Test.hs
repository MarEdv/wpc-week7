module Main where

import Test.Tasty (defaultMain,testGroup,TestTree)

import WpcWeek7.Test

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
            [ bplusSuite
            ]
