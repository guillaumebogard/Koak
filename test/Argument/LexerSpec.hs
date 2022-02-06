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
    (TestToken (AL.Filepath leftFilepath)) == (TestToken (AL.Filepath rightFilepath)) = leftFilepath == rightFilepath
    (TestToken  AL.Help)                   == (TestToken  AL.Help)                    = True
    _                                      == _                                       = False

spec :: Spec
spec = do
    it "Empty tokens" $ do
        map TestToken (tokenizeArguments [])
            == []
    it "Short help flag" $ do
        map TestToken (tokenizeArguments ["-h"])
            == [TestToken AL.Help]
    it "Long help flag" $ do
        map TestToken (tokenizeArguments ["--help"])
            == [TestToken AL.Help]
    it "One filepath" $ do
        map TestToken (tokenizeArguments ["file.koak"])
            == [TestToken (AL.Filepath "file.koak")]
    it "Short help flag with one filepath" $ do
        map TestToken (tokenizeArguments ["-h", "file.koak"])
            == [TestToken AL.Help, TestToken (AL.Filepath "file.koak")]
    it "Long help flag with one filepath" $ do
        map TestToken (tokenizeArguments ["--help", "file.koak"])
            == [TestToken AL.Help, TestToken (AL.Filepath "file.koak")]
    it "Several filepaths" $ do
        map TestToken (tokenizeArguments ["file1.koak", "file2.koak"])
            == [TestToken (AL.Filepath "file1.koak"), TestToken (AL.Filepath "file2.koak")]
    it "Short help flag with several filepaths" $ do
        map TestToken (tokenizeArguments ["-h", "file1.koak", "file2.koak"])
            == [TestToken AL.Help, TestToken (AL.Filepath "file1.koak"), TestToken (AL.Filepath "file2.koak")]
    it "Long help flag with several filepaths" $ do
        map TestToken (tokenizeArguments ["--help", "file1.koak", "file2.koak"])
            == [TestToken AL.Help, TestToken (AL.Filepath "file1.koak"), TestToken (AL.Filepath "file2.koak")]
