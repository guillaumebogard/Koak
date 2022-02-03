--
-- EPITECH PROJECT, 2022
-- KOAK
-- File description:
-- Main
--

module Main where

import Control.Exception    ( handle )
import System.Environment   ( getArgs )

import CLIArguments.Error   ( exceptionHandler )
import CLIArguments.Handler ( handleArgs )

main :: IO ()
main = handle exceptionHandler $ getArgs >>= handleExecution . handleArgs

handleExecution :: [String] -> IO ()
handleExecution (x : _) = readFile x >>= putStrLn
handleExecution _       = putStrLn "No arguments"
