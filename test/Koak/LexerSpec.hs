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

newtype TestToken = TestToken KL.Token

instance Eq TestToken where
    (==) (TestToken (Words l))          (TestToken (Words r))               = l == r 
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
            == [TestToken KL.OpenParenthesis, TestToken KL.ClosedParenthesis, TestToken KL.Comma, TestToken KL.NotEqual, TestToken KL.Equal, TestToken KL.Assign]
    it "All simple tokens with spaces" $ do
        map TestToken (tokenizeKoak "( ) + - * / ^ >= > <= < == = != ! , : ; .")
            == [TestToken KL.OpenParenthesis, TestToken KL.ClosedParenthesis, TestToken KL.Plus, TestToken KL.Minus, TestToken KL.Multiply, TestToken KL.Divide, TestToken KL.Power,
                TestToken KL.GreaterEqual, TestToken KL.Greater, TestToken KL.LowerEqual, TestToken KL.Lower, TestToken KL.Equal,
                TestToken KL.Assign, TestToken KL.NotEqual, TestToken KL.LogicalNot, TestToken KL.Comma, TestToken KL.Colon, TestToken KL.SemiColon, TestToken KL.Dot]
    it "Simple word" $ do
        map TestToken (tokenizeKoak "hello")
            == [TestToken $ KL.Words "hello"]
    it "Few words with space" $ do
        map TestToken (tokenizeKoak "This is few WORDS")
            == [TestToken $ KL.Words "This", TestToken $ KL.Words "is", TestToken $ KL.Words "few", TestToken $ KL.Words "WORDS"]
    it "Few words with few blank symbols" $ do
        map TestToken (tokenizeKoak "\t\r\nThis \n  \t\t\t  is \r\nfew WORDS\r")
            == [TestToken $ KL.Words "This", TestToken $ KL.Words "is", TestToken $ KL.Words "few", TestToken $ KL.Words "WORDS"]
