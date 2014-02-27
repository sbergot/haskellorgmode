{-# LANGUAGE OverloadedStrings #-}

module OrgModeTest (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import OrgMode
import Control.Lens

type OrgSpec = OrgDoc -> Expectation

tests :: TestTree
tests = testGroup "orgdoc"
    [ testCase "complex" fullSpec
    ]

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
    describe "complex doc" $ do

        it "can be parsed successfully" $
            withDoc "evaluations.org" $ const ok

        it "can parse tags" $
            withDoc "tags.org" $ const ok

        it "should have four children" $
            withDoc "evaluations.org" $ \d -> do
                (length $ _olChildren $ _odOutline d) `shouldBe` 4

        it "should have the correct title" $
            withDoc "evaluations.org" $ \d ->
                (view (odOutline . olTitle) d) `shouldBe` "evaluations"

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
