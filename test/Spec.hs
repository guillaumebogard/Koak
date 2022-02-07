--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Spec
--

module Main               ( main ) where

import Test.Hspec         ( Spec
                          , hspec
                          , describe )

import Argument.LexerSpec ( spec )

main :: IO ()
main = hspec Main.spec

spec :: Spec
spec = do
    describe "Argument.Lexer"  Argument.LexerSpec.spec
