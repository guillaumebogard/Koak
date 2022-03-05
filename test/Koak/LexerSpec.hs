--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.LexerSpec
--

module Koak.LexerSpec   ( spec ) where

import Test.Hspec       ( Spec
                        , it
                        , shouldThrow
                        )

import Koak.Lexer as KL ( Token(..)
                        , tokenizeKoak
                        )

spec :: Spec
spec = do
    it "One simple token" $ do
        tokenizeKoak "("
            == [
                KL.OpenParenthesis
            ]
    it "Few simple token" $ do
        tokenizeKoak "(),!====%^/"
            == [
                KL.OpenParenthesis,
                KL.ClosedParenthesis,
                KL.Word ",!====%^/"
            ]
    it "All simple tokens with spaces" $ do
        tokenizeKoak "( ) + - * / ^ >= > <= < == = != ! , : ; ."
            == [
                KL.OpenParenthesis,
                KL.ClosedParenthesis,
                KL.Word "+",
                KL.Word "-",
                KL.Word "*",
                KL.Word "/",
                KL.Word "^",
                KL.Word ">=",
                KL.Word ">",
                KL.Word "<=",
                KL.Word "<",
                KL.Word "==",
                KL.Word "=",
                KL.Word "!=",
                KL.Word "!",
                KL.Word ",",
                KL.Colon,
                KL.SemiColon ,
                KL.Word "."
            ]
    it "Simple word" $ do
        tokenizeKoak "hello"
            == [
                KL.Word "hello"
            ]
    it "Few words with spaces" $ do
        tokenizeKoak "These are few WORDS"
            == [
                KL.Word "These",
                KL.Word "are",
                KL.Word "few",
                KL.Word "WORDS"
            ]
    it "Few words with few blank symbols" $ do
        tokenizeKoak "\t\r\nThese \n  \t\t\t  are \r\nfew WORDS\r"
            == [
                KL.Word "These",
                KL.Word "are",
                KL.Word "few",
                KL.Word "WORDS"
            ]
    it "Few words with few numbers" $ do
        tokenizeKoak "These3.1 are+1=(.few12.=WORDS!\r"
            == [
                KL.Word "These3",
                KL.FloatingNumber 0.1,
                KL.Word "are",
                KL.Word "+",
                KL.IntegerNumber 1,
                KL.Word "=",
                KL.OpenParenthesis,
                KL.Word ".",
                KL.Word "few12",
                KL.Word ".=",
                KL.Word "WORDS",
                KL.Word "!"
            ]
    it "Simple number (integer)" $ do
        tokenizeKoak "3"
            == [
                KL.IntegerNumber 3
            ]
    it "Large number (integer)" $ do
        tokenizeKoak "2147483647"
            == [
                KL.IntegerNumber 2147483647
            ]
    it "Simple number (float)" $ do
        tokenizeKoak "3.14"
            == [
                KL.FloatingNumber 3.14
            ]
    it "Simple number 2 (float)" $ do
        tokenizeKoak ".1618033988749"
            == [
                KL.FloatingNumber 0.1618033988749
            ]
    it "Few numbers with few blank symbols" $ do
        tokenizeKoak "\t\r45.0 \n  \t\t\t  7874583 \r\n10.00058 .1778\r"
            == [
                KL.FloatingNumber 45.0,
                KL.IntegerNumber 7874583,
                KL.FloatingNumber 10.00058,
                KL.FloatingNumber 0.1778
            ]
    it "Few numbers with few simple tokens" $ do
        tokenizeKoak "==3545.15<=. <1129>=(.58=8!\r"
            == [
                KL.Word "==",
                KL.FloatingNumber 3545.15,
                KL.Word "<=.",
                KL.Word "<",
                KL.IntegerNumber 1129,
                KL.Word ">=",
                KL.OpenParenthesis,
                KL.FloatingNumber 0.58,
                KL.Word "=",
                KL.IntegerNumber 8,
                KL.Word "!"
            ]
    it "Real world basic example" $ do
        tokenizeKoak (
            "extern putchard(char);"                                    ++
            "def printdensity(d)"                                       ++
            "  if d > 8 then"                                           ++
            "    putchard(32)"                                          ++
            "  else if d > 4 then"                                      ++
            "    putchard(46)"                                          ++
            "  else if d > 2 then"                                      ++
            "    putchard(43)"                                          ++
            "  else"                                                    ++
            "    putchard(42);"                                         ++
            "\n\n"                                                      ++
            "       printdensity(1): printdensity(2): printdensity(3):" ++
            "       printdensity(4): printdensity(5): printdensity(9):" ++
            "       putchard(10);"
            )
            == [
                KL.Word "extern",
                KL.Word "putchard",
                KL.OpenParenthesis,
                KL.Word "char",
                KL.ClosedParenthesis,
                KL.SemiColon,
                KL.Word "def",
                KL.Word "printdensity",
                KL.OpenParenthesis,
                KL.Word "d",
                KL.ClosedParenthesis,
                KL.Word "if",
                KL.Word "d",
                KL.Word ">",
                KL.IntegerNumber 8,
                KL.Word "then",
                KL.Word "putchard",
                KL.OpenParenthesis,
                KL.IntegerNumber 32,
                KL.ClosedParenthesis,
                KL.Word "else",
                KL.Word "if",
                KL.Word "d",
                KL.Word ">",
                KL.IntegerNumber 4,
                KL.Word "then",
                KL.Word "putchard",
                KL.OpenParenthesis,
                KL.IntegerNumber 46,
                KL.ClosedParenthesis,
                KL.Word "else",
                KL.Word "if",
                KL.Word "d",
                KL.Word ">",
                KL.IntegerNumber 2,
                KL.Word "then",
                KL.Word "putchard",
                KL.OpenParenthesis,
                KL.IntegerNumber 43,
                KL.ClosedParenthesis,
                KL.Word "else",
                KL.Word "putchard",
                KL.OpenParenthesis,
                KL.IntegerNumber 42,
                KL.ClosedParenthesis,
                KL.SemiColon,
                KL.Word "printdensity",
                KL.OpenParenthesis,
                KL.IntegerNumber 1,
                KL.ClosedParenthesis,
                KL.Colon,
                KL.Word "printdensity",
                KL.OpenParenthesis,
                KL.IntegerNumber 2,
                KL.ClosedParenthesis,
                KL.Colon,
                KL.Word "printdensity",
                KL.OpenParenthesis,
                KL.IntegerNumber 3,
                KL.ClosedParenthesis,
                KL.Colon,
                KL.Word "printdensity",
                KL.OpenParenthesis,
                KL.IntegerNumber 4,
                KL.ClosedParenthesis,
                KL.Colon,
                KL.Word "printdensity",
                KL.OpenParenthesis,
                KL.IntegerNumber 5,
                KL.ClosedParenthesis,
                KL.Colon,
                KL.Word "printdensity",
                KL.OpenParenthesis,
                KL.IntegerNumber 9,
                KL.ClosedParenthesis,
                KL.Colon,
                KL.Word "putchard",
                KL.OpenParenthesis,
                KL.IntegerNumber 10,
                KL.ClosedParenthesis,
                KL.SemiColon
            ]
    it "Only not usable characters" $
        tokenizeKoak "\r��"
            == [
                KL.Word "��"
            ]
    it "Only space characters" $
        null $ tokenizeKoak "\r\t"
