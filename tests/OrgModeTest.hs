{-# LANGUAGE OverloadedStrings #-}

module OrgModeTest (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import OrgMode
import Control.Lens

type OrgSpec = OrgDoc -> Expectation

tests :: TestTree
tests = testCase "" fullSpec

dataPath :: String
dataPath = "tests\\data\\"

withDoc :: String -> OrgSpec -> Expectation
withDoc src spec = do
    d <- parseOrgFile (dataPath ++ src) ["TODO"]
    either (parseFail . show) spec d
  where
    parseFail msg = expectationFailure $
        "Failed to parse \"" ++ src ++ "\"\n" ++ "Error: \n" ++ msg

ok :: Expectation
ok = return ()

fullSpec :: Spec
fullSpec = do

    describe "parse docs" $ do

        it "can be parsed successfully" $
            withDoc "evaluations.org" $ const ok

        it "can parse tags" $
            withDoc "tags.org" $ const ok

    describe "basic data" $ do

        it "should have four children" $
            withDoc "evaluations.org" $ \d -> do
                (length $ view (odOutline . olChildren) d) `shouldBe` 4

        it "should have the correct title" $
            withDoc "evaluations.org" $ \d ->
                (view (odOutline . olTitle) d) `shouldBe` "evaluations"

    describe "tags" $ do

        it "should be parsed" $
            withDoc "tags.org" $ \d -> do
                case view (odOutline . olTags) d of
                    [_] -> ok
                    m  -> expectationFailure $ "wrong structure: \n" ++ (show m)

        it "should be parsed" $
            withDoc "tags.org" $ \d -> do
                (length $ view olTags $ head $ view (odOutline . olChildren) d) `shouldBe` 3

    describe "lists" $ do

        it "should parse lists" $
            withDoc "lists.org" $ \d -> case (view (odOutline . olText) d) of
                [ListBlock _] -> ok
                _ -> expectationFailure $ "list not found: \n" ++ (show d)

        it "should handle mixed paragraphs/lists" $ withDoc "mix.org" $ \d ->
            case view (odOutline . olText) d of
                [Paragraph _, ListBlock _] -> ok
                m -> expectationFailure $ "wrong structure: \n" ++ (show m)

        it "should handle multiple mixed paragraphs/lists" $ withDoc "mix_multiple.org" $ \d ->
            case view (odOutline . olText) d of
                [Paragraph _, ListBlock _, Paragraph _, Paragraph _, ListBlock _] -> ok
                m -> expectationFailure $ "wrong structure: \n" ++ (show m)

    describe "closed date parser" $ do

        it "should parse nothing if no date" $ withDoc "evaluations.org" $ \d ->
            case view (odOutline . olCloseDate) d of
                Nothing -> ok
                m -> expectationFailure $ "wrong structure: \n" ++ (show m)

        it "should parse close dates" $ withDoc "closeddate.org" $ \d ->
            case view (odOutline . olCloseDate) d of
                Just "2013-08-09 ven. 11:52" -> ok
                m -> expectationFailure $ "wrong structure: \n" ++ (show m)

    describe "properties parser" $ do

        it "should parse properties" $ withDoc "properties.org" $ \d ->
            case view (odOutline . olProperties) d of
                [_, _] -> ok
                m -> expectationFailure $ "wrong structure: \n" ++ (show m)

        it "should parse text after props" $ withDoc "properties.org" $ \d ->
            case view (odOutline . olText) d of
                [Paragraph "yolo!"] -> ok
                m -> expectationFailure $ "wrong structure: \n" ++ (show m)
