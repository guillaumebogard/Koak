--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.LexerSpec
--

module Koak.LexerSpec   ( spec ) where

import Test.Hspec       ( Spec
                        , it, shouldBe
                        )

import Koak.Lexer as KL ( Token(..)
                        , tokenizeKoak
                        )
import qualified Data.Ord as KL
import qualified GHC.IO.Handle.Internals as KL

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
    it "Empty tokens" $ do
        map TestToken (tokenizeKoak "")
            == []
    it "One simple token" $ do
        map TestToken (tokenizeKoak "(")
            == [TestToken KL.OpenParenthesis]
    it "Few simple token" $ do
        map TestToken (tokenizeKoak "(),!====")
            == [TestToken KL.OpenParenthesis,
                TestToken KL.ClosedParenthesis,
                TestToken KL.Comma,
                TestToken KL.NotEqual,
                TestToken KL.Equal,
                TestToken KL.Assign]
    it "All simple tokens with spaces" $ do
        map TestToken (tokenizeKoak "( ) + - * / ^ >= > <= < == = != ! , : ; .")
            == [TestToken KL.OpenParenthesis,
                TestToken KL.ClosedParenthesis,
                TestToken KL.Plus,
                TestToken KL.Minus,
                TestToken KL.Multiply,
                TestToken KL.Divide,
                TestToken KL.Power,
                TestToken KL.GreaterEqual,
                TestToken KL.Greater,
                TestToken KL.LowerEqual,
                TestToken KL.Lower,
                TestToken KL.Equal,
                TestToken KL.Assign,
                TestToken KL.NotEqual,
                TestToken KL.LogicalNot,
                TestToken KL.Comma,
                TestToken KL.Colon,
                TestToken KL.SemiColon,
                TestToken KL.Dot]
    it "Simple word" $ do
        map TestToken (tokenizeKoak "hello")
            == [TestToken $ KL.Word "hello"]
    it "Few words with space" $ do
        map TestToken (tokenizeKoak "This is few WORDS")
            == [TestToken $ KL.Word "This",
                TestToken $ KL.Word "is",
                TestToken $ KL.Word "few",
                TestToken $ KL.Word "WORDS"]
    it "Few words with few blank symbols" $ do
        map TestToken (tokenizeKoak "\t\r\nThis \n  \t\t\t  is \r\nfew WORDS\r")
            == [TestToken $ KL.Word "This",
                TestToken $ KL.Word "is",
                TestToken $ KL.Word "few",
                TestToken $ KL.Word "WORDS"]
    it "Few words with few simple tokens" $ do
        map TestToken (tokenizeKoak "==This<=. <is>=(.few12.=WORDS!\r")
            == [TestToken KL.Equal,
                TestToken $ KL.Word "This",
                TestToken KL.LowerEqual,
                TestToken KL.Dot,
                TestToken KL.Lower,
                TestToken $ KL.Word "is",
                TestToken KL.GreaterEqual,
                TestToken KL.OpenParenthesis,
                TestToken KL.Dot,
                TestToken $ KL.Word "few12",
                TestToken KL.Dot,
                TestToken KL.Assign,
                TestToken $ KL.Word "WORDS",
                TestToken KL.LogicalNot]
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
            == [TestToken $ KL.Word "extern",
                TestToken $ KL.Word "putchard",
                TestToken KL.OpenParenthesis,
                TestToken $ KL.Word "char",
                TestToken KL.ClosedParenthesis,
                TestToken KL.SemiColon,
                TestToken $ KL.Word "def",
                TestToken $ KL.Word "printdensity",
                TestToken KL.OpenParenthesis,
                TestToken $ KL.Word "d",
                TestToken KL.ClosedParenthesis,
                TestToken $ KL.Word "if",
                TestToken $ KL.Word "d",
                TestToken KL.Greater,
                TestToken $ KL.Number 8,
                TestToken $ KL.Word "then",
                TestToken $ KL.Word "putchard",
                TestToken KL.OpenParenthesis,
                TestToken $ KL.Number 32,
                TestToken KL.ClosedParenthesis,
                TestToken $ KL.Word "else",
                TestToken $ KL.Word "if",
                TestToken $ KL.Word "d",
                TestToken KL.Greater,
                TestToken $ KL.Number 4,
                TestToken $ KL.Word "then",
                TestToken $ KL.Word "putchard",
                TestToken KL.OpenParenthesis,
                TestToken $ KL.Number 46,
                TestToken KL.ClosedParenthesis,
                TestToken $ KL.Word "else",
                TestToken $ KL.Word "if",
                TestToken $ KL.Word "d",
                TestToken KL.Greater,
                TestToken $ KL.Number 2,
                TestToken $ KL.Word "then",
                TestToken $ KL.Word "putchard",
                TestToken KL.OpenParenthesis,
                TestToken $ KL.Number 43,
                TestToken KL.ClosedParenthesis,
                TestToken $ KL.Word "else",
                TestToken $ KL.Word "putchard",
                TestToken KL.OpenParenthesis,
                TestToken $ KL.Number 42,
                TestToken KL.ClosedParenthesis,
                TestToken KL.SemiColon,
                TestToken $ KL.Word "printdensity",
                TestToken KL.OpenParenthesis,
                TestToken $ KL.Number 1,
                TestToken KL.ClosedParenthesis,
                TestToken KL.Colon,
                TestToken $ KL.Word "printdensity",
                TestToken KL.OpenParenthesis,
                TestToken $ KL.Number 2,
                TestToken KL.ClosedParenthesis,
                TestToken KL.Colon,
                TestToken $ KL.Word "printdensity",
                TestToken KL.OpenParenthesis,
                TestToken $ KL.Number 3,
                TestToken KL.ClosedParenthesis,
                TestToken KL.Colon,
                TestToken $ KL.Word "printdensity",
                TestToken KL.OpenParenthesis,
                TestToken $ KL.Number 4,
                TestToken KL.ClosedParenthesis,
                TestToken KL.Colon,
                TestToken $ KL.Word "printdensity",
                TestToken KL.OpenParenthesis,
                TestToken $ KL.Number 5,
                TestToken KL.ClosedParenthesis,
                TestToken KL.Colon,
                TestToken $ KL.Word "printdensity",
                TestToken KL.OpenParenthesis,
                TestToken $ KL.Number 9,
                TestToken KL.ClosedParenthesis,
                TestToken KL.Colon,
                TestToken $ KL.Word "putchard",
                TestToken KL.OpenParenthesis,
                TestToken $ KL.Number 10,
                TestToken KL.ClosedParenthesis,
                TestToken KL.SemiColon]

-- ready> ;
-- ...
