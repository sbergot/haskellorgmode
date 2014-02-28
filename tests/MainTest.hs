module Main ( main ) where

import Test.Tasty

import qualified OrgModeTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "" [OrgModeTest.tests]