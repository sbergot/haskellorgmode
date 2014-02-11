{-# LANGUAGE OverloadedStrings #-}

module OrgModeTest (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import OrgMode

import qualified Data.Text.IO as TIO

type OrgSpec = OrgDoc -> Expectation

tests :: TestTree
tests = testGroup "animals"
    [ testCase "complex" fullSpec
    ]

dataPath :: String
dataPath = "tests\\data\\"

withDoc :: String -> OrgSpec -> Expectation
withDoc src spec = do
    t <- TIO.readFile $ dataPath ++ src
    either (parseFail . show) spec $ parseRes t
  where
    parseRes t = parseOrgDoc ["TODO"] src t
    parseFail msg = expectationFailure $
        "Failed to parse \"" ++ src ++ "\"\n" ++ "Error: \n" ++ msg

parseSuccess :: OrgSpec
parseSuccess = const $ return ()

firstChildren :: OrgSpec
firstChildren doc = (length $ _olChildren $ _odOutline doc) `shouldBe` 4

title :: OrgSpec
title doc = (_olTitle $ _odOutline doc) `shouldBe` "evaluations"

fullSpec :: Spec
fullSpec = do
    describe "complex doc" $ do
        it "can be parsed successfully" $
            withDoc "evaluations.org" parseSuccess
        it "can parse tags" $
            withDoc "tags.org" parseSuccess
        it "should have four children" $
            withDoc "evaluations.org" firstChildren
        it "should have the correct title" $
            withDoc "evaluations.org" title
--        it "print" $
--            withDoc "evaluations.org" $ print . _odOutline
