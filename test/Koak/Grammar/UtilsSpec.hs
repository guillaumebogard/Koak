--
-- EPITECH PROJECT, 2022
-- koak [WSL: Manjaro]
-- File description:
-- Koak.Grammar.UtilsSpec
--

module Koak.Grammar.UtilsSpec ( spec ) where

import Test.Hspec       ( Spec
                        , it
                        )

import Koak.Grammar.Utils

spec :: Spec
spec = do
    it "isSyntaxToken: simple correct token '('" $ do
        isSyntaxToken '('
    it "isSyntaxToken: simple wrong token 'w'" $ do
        not $ isSyntaxToken 'w'
    it "isAlphaWord: simple correct alpha" $ do
        isAlphaWord "a"
    it "isAlphaWord: simple wrong alpha" $ do
        not $ isAlphaWord "a++"
    it "isAlphaWord: complex correct alpha" $ do
        isAlphaWord "abcde"
    it "isAlphaWord: complex wrong alpha" $ do
        not $ isAlphaWord "acb>"
    it "isAlphaNumWord: simple correct alphanum" $ do
        isAlphaNumWord "a"
    it "isAlphaNumWord: simple wrong alphanum" $ do
        not $ isAlphaNumWord "a++"
    it "isAlphaNumWord: complex correct alphanum" $ do
        isAlphaNumWord "abcde_'"
    it "isAlphaNumWord: complex wrong alphanum" $ do
        not $ isAlphaNumWord "___acb>"
    it "isSpecialWord: simple correct operator" $ do
        isSpecialWord "+"
    it "isSpecialWord: simple wrong operator" $ do
        not $ isSpecialWord "test"
    it "isSpecialWord: complex correct operator" $ do
        isSpecialWord ">>**$>"
    it "isSpecialWord: complex wrong operator" $ do
        not $ isSpecialWord "__test''"
