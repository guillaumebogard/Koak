--
-- EPITECH PROJECT, 2022
-- KOAK
-- File description:
-- Main
--

module Main where

import CLIArguments.Error (exceptionHandler)
import CLIArguments.Handler (handleArgs)
import Control.Exception (catch)
import System.Environment (getArgs)

main :: IO ()
main = catch (getArgs >>= handleExecution . handleArgs) exceptionHandler

handleExecution :: [String] -> IO ()
handleExecution (x : _) = readFile x >>= \content -> putStrLn content
handleExecution _       = putStrLn "No arguments"
