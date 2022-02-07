--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Argument.ParserSpec
--

module Argument.ParserSpec   ( spec ) where

import Control.Exception     ( evaluate )
import Test.Hspec            ( Spec
                             , it
                             , shouldThrow
                             )

import Error                 ( KoakError(KoakArgumentParserError, KoakHelpError) )
import Argument.Parser as AP ( Filepath(..)
                             , KoakArguments(..)
                             , parseArguments
                             )

instance Eq KoakError where
    (KoakArgumentParserError _) == (KoakArgumentParserError _) = True
    KoakHelpError               == KoakHelpError               = True
    _                           == _                           = False

newtype TestKoakArguments = TestKoakArguments AP.KoakArguments
instance Eq TestKoakArguments where
    (TestKoakArguments (KoakArguments (AP.Filepath leftFilepath))) == (TestKoakArguments (KoakArguments (AP.Filepath rightFilepath))) = leftFilepath == rightFilepath

spec :: Spec
spec = do
    it "Empty arguments" $ do
        evaluate (parseArguments [])
            `shouldThrow` (== KoakArgumentParserError "Incorrect arguments, retry with -h to display usage")
    it "Several filepaths" $ do
        evaluate (parseArguments ["file1.koak", "file2.koak"])
            `shouldThrow` (== KoakArgumentParserError "Incorrect arguments, retry with -h to display usage")
    it "Short help flag" $ do
        evaluate (parseArguments ["-h"])
            `shouldThrow` (== KoakHelpError)
    it "Long help flag" $ do
        evaluate (parseArguments ["--help"])
            `shouldThrow` (== KoakHelpError)
    it "Show instance KoakArgumentParserError" $ do
        show (KoakArgumentParserError "Incorrect file")
            == "Argument Parser Error: Incorrect file"
    it "Show instance KoakHelpError" $ do
        show KoakHelpError
            == "Usage: ./koak file\n" ++
               "Description:\n" ++
               "\tAn interpreter of the KOAK language.\n" ++
               "Options:\n" ++
               "\t--help\t\tDisplay this information."
    it "One filepath" $ do
        TestKoakArguments (parseArguments ["file1.koak"])
            == TestKoakArguments (KoakArguments (Filepath "file1.koak"))
