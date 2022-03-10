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
            `shouldThrow` (== APE.KoakArgumentParserException "Requires at least 1 filepath")
    it "Several filepaths" $
        evaluate (AP.parseArguments ["file1.koak", "file2.koak"])
            `shouldThrow` (== APE.KoakArgumentParserException "Can only take 1 filepath")
    it "Short help flag" $
        evaluate (AP.parseArguments ["-h"])
            `shouldThrow` (== APE.KoakHelpException)
    it "Long help flag" $
        evaluate (AP.parseArguments ["--help"])
            `shouldThrow` (== APE.KoakHelpException)
    it "Unknown short option with one filepath" $
        evaluate (AP.parseArguments ["-a", "file1.koak"])
            `shouldThrow` (== APE.KoakArgumentParserException "Unknown option '-a'")
    it "Unknown short option with one filepath reversed" $
        evaluate (AP.parseArguments ["file1.koak", "-a"])
            `shouldThrow` (== APE.KoakArgumentParserException "Unknown option '-a'")
    it "One filepath" $
        TestKoakArguments (AP.parseArguments ["file1.koak"])
            == TestKoakArguments (AP.KoakArguments (AP.Filepath "file1.koak"))
