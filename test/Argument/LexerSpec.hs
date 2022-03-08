--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Argument.LexerSpec
--

module Argument.LexerSpec   ( spec ) where

import Test.Hspec           ( Spec
                            , it
                            )

import Argument.Lexer as AL ( Token(..)
                            , tokenizeArguments
                            )

newtype TestToken = TestToken AL.Token
instance Eq TestToken where
    (TestToken (AL.Filepath      leftFilepath)) == (TestToken (AL.Filepath      rightFilepath)) = leftFilepath == rightFilepath
    (TestToken (AL.UnknownOption leftOption))   == (TestToken (AL.UnknownOption rightOption))   = leftOption   == rightOption
    (TestToken  AL.Help)                        == (TestToken  AL.Help)                         = True
    _                                           == _                                            = False

spec :: Spec
spec = do
    it "Empty tokens" $
        null $ map TestToken $ tokenizeArguments []
    it "Short help option" $
        map TestToken (tokenizeArguments ["-h"])
            == map TestToken [AL.Help]
    it "Long help option" $
        map TestToken (tokenizeArguments ["--help"])
            == map TestToken [AL.Help]
    it "One filepath" $
        map TestToken (tokenizeArguments ["file.koak"])
            == map TestToken [AL.Filepath "file.koak"]
    it "Short help option with one filepath" $
        map TestToken (tokenizeArguments ["-h", "file.koak"])
            == map TestToken [AL.Help, AL.Filepath "file.koak"]
    it "Long help option with one filepath" $
        map TestToken (tokenizeArguments ["--help", "file.koak"])
            == map TestToken [AL.Help, AL.Filepath "file.koak"]
    it "Several filepaths" $
        map TestToken (tokenizeArguments ["file1.koak", "file2.koak"])
            == map TestToken [AL.Filepath "file1.koak", AL.Filepath "file2.koak"]
    it "Short help option with several filepaths" $
        map TestToken (tokenizeArguments ["-h", "file1.koak", "file2.koak"])
            == map TestToken [AL.Help, AL.Filepath "file1.koak", AL.Filepath "file2.koak"]
    it "Long help option with several filepaths" $
        map TestToken (tokenizeArguments ["--help", "file1.koak", "file2.koak"])
            == map TestToken [AL.Help, AL.Filepath "file1.koak", AL.Filepath "file2.koak"]
    it "Unknown short option" $
        map TestToken (tokenizeArguments ["-a"])
            == map TestToken [AL.UnknownOption "-a"]
    it "Unknown long option" $
        map TestToken (tokenizeArguments ["--a"])
            == map TestToken [AL.UnknownOption "--a"]
    it "Unknown short option with one filepath" $
        map TestToken (tokenizeArguments ["-a", "file1.koak"])
            == map TestToken [AL.UnknownOption "-a", AL.Filepath "file1.koak"]
    it "Unknown short option with one filepath reversed" $
        map TestToken (tokenizeArguments ["file1.koak", "-a"])
            == map TestToken [AL.Filepath "file1.koak", AL.UnknownOption "-a"]
