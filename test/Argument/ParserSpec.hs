--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Argument.ParserSpec
--

module Argument.ParserSpec                        ( spec ) where

import Control.Exception                          ( evaluate )
import Test.Hspec                                 ( Spec
                                                  , it
                                                  , shouldThrow
                                                  )

import Exception                                  ( KoakException( KoakAPE ) )
import qualified Argument.Parser.Exception as APE ( KoakArgumentParserException(..) )
import qualified Argument.Parser           as AP  ( Filepath(..)
                                                  , KoakArguments(..)
                                                  , parseArguments
                                                  )

newtype TestKoakArguments = TestKoakArguments AP.KoakArguments
instance Eq TestKoakArguments where
    (TestKoakArguments (AP.KoakArguments (AP.Filepath leftFilepath))) == (TestKoakArguments (AP.KoakArguments (AP.Filepath rightFilepath))) = leftFilepath == rightFilepath

spec :: Spec
spec = do
    it "Empty arguments" $
        evaluate (AP.parseArguments [])
            `shouldThrow` (== KoakAPE (APE.KoakArgumentParserException "Requires at least 1 filepath"))
    it "Several filepaths" $
        evaluate (AP.parseArguments ["file1.koak", "file2.koak"])
            `shouldThrow` (== KoakAPE (APE.KoakArgumentParserException "Can only take 1 filepath"))
    it "Short help flag" $
        evaluate (AP.parseArguments ["-h"])
            `shouldThrow` (== KoakAPE APE.KoakHelpException)
    it "Long help flag" $
        evaluate (AP.parseArguments ["--help"])
            `shouldThrow` (== KoakAPE APE.KoakHelpException)
    it "Unknown short option with one filepath" $
        evaluate (AP.parseArguments ["-a", "file1.koak"])
            `shouldThrow` (== KoakAPE (APE.KoakArgumentParserException "Unknown option '-a'"))
    it "Unknown short option with one filepath reversed" $
        evaluate (AP.parseArguments ["file1.koak", "-a"])
            `shouldThrow` (== KoakAPE (APE.KoakArgumentParserException "Unknown option '-a'"))
    it "Show instance KoakArgumentParserException" $
        show (APE.KoakArgumentParserException "Invalid file")
            == "Argument Parsing Exception: Invalid file"
    it "Show instance KoakHelpException" $
        show APE.KoakHelpException
            == "Usage: ./koak file\n"                     ++
               "Description:\n"                           ++
               "\tAn interpreter of the KOAK language.\n" ++
               "Options:\n"                               ++
               "\t--help\t\tDisplay this information."
    it "One filepath" $
        TestKoakArguments (AP.parseArguments ["file1.koak"])
            == TestKoakArguments (AP.KoakArguments (AP.Filepath "file1.koak"))
