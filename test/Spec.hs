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
import Koak.LexerSpec     ( spec )
import Koak.ParserSpec    ( spec )

main :: IO ()
main = hspec Main.spec

spec :: Spec
spec = do
    describe "Argument.Lexer"  Argument.LexerSpec.spec
    describe "Koak.Lexer"      Koak.LexerSpec.spec
    describe "Koak.Parser"     Koak.ParserSpec.spec
