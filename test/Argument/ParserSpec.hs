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

import Exception             ( KoakException(KoakArgumentParserException, KoakHelpException) )
import Argument.Parser as AP ( Filepath(..)
                             , KoakArguments(..)
                             , parseArguments
                             )

newtype TestKoakArguments = TestKoakArguments AP.KoakArguments
instance Eq TestKoakArguments where
    (TestKoakArguments (KoakArguments (AP.Filepath leftFilepath))) == (TestKoakArguments (KoakArguments (AP.Filepath rightFilepath))) = leftFilepath == rightFilepath

spec :: Spec
spec = do
    it "Empty arguments" $ do
        evaluate (parseArguments [])
            `shouldThrow` (== KoakArgumentParserException "Invalid arguments, retry with -h to display usage")
    it "Several filepaths" $ do
        evaluate (parseArguments ["file1.koak", "file2.koak"])
            `shouldThrow` (== KoakArgumentParserException "Invalid arguments, retry with -h to display usage")
    it "Short help flag" $ do
        evaluate (parseArguments ["-h"])
            `shouldThrow` (== KoakHelpException)
    it "Long help flag" $ do
        evaluate (parseArguments ["--help"])
            `shouldThrow` (== KoakHelpException)
    it "Unknown short option with one filepath" $ do
        evaluate (parseArguments ["-a", "file1.koak"])
            `shouldThrow` (== KoakArgumentParserException "Invalid arguments, retry with -h to display usage")
    it "Unknown short option with one filepath reversed" $ do
        evaluate (parseArguments ["file1.koak", "-a"])
            `shouldThrow` (== KoakArgumentParserException "Invalid arguments, retry with -h to display usage")
    it "Show instance KoakArgumentParserException" $ do
        show (KoakArgumentParserException "Invalid file")
            == "Argument Parser Exception: Invalid file"
    it "Show instance KoakHelpException" $ do
        show KoakHelpException
            == "Usage: ./koak file\n"                     ++
               "Description:\n"                           ++
               "\tAn interpreter of the KOAK language.\n" ++
               "Options:\n"                               ++
               "\t--help\t\tDisplay this information."
    it "One filepath" $ do
        TestKoakArguments (parseArguments ["file1.koak"])
            == TestKoakArguments (KoakArguments (Filepath "file1.koak"))
