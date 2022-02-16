--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.LexerSpec
--

module Koak.LexerSpec   ( spec ) where

import Test.Hspec       ( Spec
                        , it, shouldBe, shouldThrow, anyException
                        )

import Koak.Lexer as KL ( Token(..)
                        , tokenizeKoak
                        )

import qualified Data.Ord
import qualified GHC.IO.Handle.Internals

newtype TestToken = TestToken KL.Token

instance Eq TestToken where
    (==) (TestToken (Word l))           (TestToken (Word r))                = l == r 
    (==) (TestToken (Number l))         (TestToken (Number r))              = l == r
    (==) (TestToken OpenParenthesis)    (TestToken OpenParenthesis)         = True
    (==) (TestToken ClosedParenthesis)  (TestToken ClosedParenthesis)       = True
    (==) (TestToken Plus)               (TestToken Plus)                    = True
    (==) (TestToken Minus)              (TestToken Minus)                   = True
    (==) (TestToken Multiply)           (TestToken Multiply)                = True
    (==) (TestToken Divide)             (TestToken Divide)                  = True
    (==) (TestToken Modulo)             (TestToken Modulo)                  = True
    (==) (TestToken Power)              (TestToken Power)                   = True
    (==) (TestToken Greater)            (TestToken Greater)                 = True
    (==) (TestToken GreaterEqual)       (TestToken GreaterEqual)            = True
    (==) (TestToken Lower)              (TestToken Lower)                   = True
    (==) (TestToken LowerEqual)         (TestToken LowerEqual)              = True
    (==) (TestToken Equal)              (TestToken Equal)                   = True
    (==) (TestToken NotEqual)           (TestToken NotEqual)                = True
    (==) (TestToken LogicalNot)         (TestToken LogicalNot)              = True
    (==) (TestToken Assign)             (TestToken Assign)                  = True
    (==) (TestToken Comma)              (TestToken Comma)                   = True
    (==) (TestToken Colon)              (TestToken Colon)                   = True
    (==) (TestToken SemiColon)          (TestToken SemiColon)               = True
    (==) (TestToken Dot)                (TestToken Dot)                     = True
    (==) _                              _                                   = False

spec :: Spec
spec = do
    it "One simple token" $ do
        map TestToken (tokenizeKoak "(")
            == map TestToken [
                KL.OpenParenthesis
            ]
    it "Few simple token" $ do
        map TestToken (tokenizeKoak "(),!====%^/")
            == map TestToken [
                KL.OpenParenthesis,
                KL.ClosedParenthesis,
                KL.Comma,
                KL.NotEqual,
                KL.Equal,
                KL.Assign,
                KL.Modulo,
                KL.Power,
                KL.Divide
            ]
    it "All simple tokens with spaces" $ do
        map TestToken (tokenizeKoak "( ) + - * / ^ >= > <= < == = != ! , : ; .")
            == map TestToken [
                KL.OpenParenthesis,
                KL.ClosedParenthesis,
                KL.Plus,
                KL.Minus,
                KL.Multiply,
                KL.Divide,
                KL.Power,
                KL.GreaterEqual,
                KL.Greater,
                KL.LowerEqual,
                KL.Lower,
                KL.Equal,
                KL.Assign,
                KL.NotEqual,
                KL.LogicalNot,
                KL.Comma,
                KL.Colon,
                KL.SemiColon,
                KL.Dot
            ]
    it "Simple word" $ do
        map TestToken (tokenizeKoak "hello")
            == map TestToken [
                KL.Word "hello"
            ]
    it "Few words with space" $ do
        map TestToken (tokenizeKoak "This is few WORDS")
            == map TestToken [
                KL.Word "This",
                KL.Word "is",
                KL.Word "few",
                KL.Word "WORDS"
            ]
    it "Few words with few blank symbols" $ do
        map TestToken (tokenizeKoak "\t\r\nThis \n  \t\t\t  is \r\nfew WORDS\r")
            == map TestToken [
                KL.Word "This",
                KL.Word "is",
                KL.Word "few",
                KL.Word "WORDS"
            ]
    it "Few words with few simple tokens" $ do
        map TestToken (tokenizeKoak "==This<=. <is>=(.few12.=WORDS!\r")
            == map TestToken [
                KL.Equal,
                KL.Word "This",
                KL.LowerEqual,
                KL.Dot,
                KL.Lower,
                KL.Word "is",
                KL.GreaterEqual,
                KL.OpenParenthesis,
                KL.Dot,
                KL.Word "few12",
                KL.Dot,
                KL.Assign,
                KL.Word "WORDS",
                KL.LogicalNot
            ]
    it "Simple number (integer)" $ do
        map TestToken (tokenizeKoak "3")
            == map TestToken [
                KL.Number 3
            ]
    it "Large number (integer)" $ do
        map TestToken (tokenizeKoak "2147483647")
            == map TestToken [
                KL.Number 2147483647
            ]
    it "Simple number (float)" $ do
        map TestToken (tokenizeKoak "3.14")
            == map TestToken [
                KL.Number 3.14
            ]
    it "Simple number 2 (float)" $ do
        map TestToken (tokenizeKoak ".1618033988749")
            == map TestToken [
                KL.Number 0.1618033988749
            ]
    it "Few numbers with few blank symbols" $ do
        map TestToken (tokenizeKoak "\t\r45.0 \n  \t\t\t  7874583 \r\n10.00058 .1778\r")
            == map TestToken [
                KL.Number 45.0,
                KL.Number 7874583,
                KL.Number 10.00058,
                KL.Number 0.1778
            ]
    it "Few numbers with few simple tokens" $ do
        map TestToken (tokenizeKoak "==3545.15<=. <1129>=(.58=8!\r")
            == map TestToken [
                KL.Equal,
                KL.Number 3545.15,
                KL.LowerEqual,
                KL.Dot,
                KL.Lower,
                KL.Number 1129,
                KL.GreaterEqual,
                KL.OpenParenthesis,
                KL.Number 0.58,
                KL.Assign,
                KL.Number 8,
                KL.LogicalNot
            ]
    it "Real world basic example" $ do
        map TestToken (
            tokenizeKoak $
            "extern putchard(char);" ++
            "def printdensity(d)"                                           ++
            "  if d > 8 then"                                               ++
            "    putchard(32)"                                              ++
            "  else if d > 4 then"                                          ++
            "    putchard(46)"                                              ++
            "  else if d > 2 then"                                          ++
            "    putchard(43)"                                              ++
            "  else"                                                        ++
            "    putchard(42);"                                             ++
            "\n\n"                                                          ++
            "       printdensity(1): printdensity(2): printdensity(3):"     ++
            "       printdensity(4): printdensity(5): printdensity(9):"     ++
            "       putchard(10);"
            )
            == map TestToken [
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
                KL.Greater,
                KL.Number 8,
                KL.Word "then",
                KL.Word "putchard",
                KL.OpenParenthesis,
                KL.Number 32,
                KL.ClosedParenthesis,
                KL.Word "else",
                KL.Word "if",
                KL.Word "d",
                KL.Greater,
                KL.Number 4,
                KL.Word "then",
                KL.Word "putchard",
                KL.OpenParenthesis,
                KL.Number 46,
                KL.ClosedParenthesis,
                KL.Word "else",
                KL.Word "if",
                KL.Word "d",
                KL.Greater,
                KL.Number 2,
                KL.Word "then",
                KL.Word "putchard",
                KL.OpenParenthesis,
                KL.Number 43,
                KL.ClosedParenthesis,
                KL.Word "else",
                KL.Word "putchard",
                KL.OpenParenthesis,
                KL.Number 42,
                KL.ClosedParenthesis,
                KL.SemiColon,
                KL.Word "printdensity",
                KL.OpenParenthesis,
                KL.Number 1,
                KL.ClosedParenthesis,
                KL.Colon,
                KL.Word "printdensity",
                KL.OpenParenthesis,
                KL.Number 2,
                KL.ClosedParenthesis,
                KL.Colon,
                KL.Word "printdensity",
                KL.OpenParenthesis,
                KL.Number 3,
                KL.ClosedParenthesis,
                KL.Colon,
                KL.Word "printdensity",
                KL.OpenParenthesis,
                KL.Number 4,
                KL.ClosedParenthesis,
                KL.Colon,
                KL.Word "printdensity",
                KL.OpenParenthesis,
                KL.Number 5,
                KL.ClosedParenthesis,
                KL.Colon,
                KL.Word "printdensity",
                KL.OpenParenthesis,
                KL.Number 9,
                KL.ClosedParenthesis,
                KL.Colon,
                KL.Word "putchard",
                KL.OpenParenthesis,
                KL.Number 10,
                KL.ClosedParenthesis,
                KL.SemiColon
            ]

-- ready> ;
-- ...
