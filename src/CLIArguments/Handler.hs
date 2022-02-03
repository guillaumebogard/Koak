--
-- EPITECH PROJECT, 2022
-- KOAK
-- File description:
-- CLIArguments Lexer
--

module CLIArguments.Handler ( handleArgs ) where

import Control.Exception    ( throw )

import CLIArguments.Error   ( ArgException( InvalidOption, Help ) )

handleArgs :: [String] -> [String]
handleArgs[]                  = []
handleArgs (('-' : opt) : xs) = handleOption opt : handleArgs xs
handleArgs (x           : xs) = x : handleArgs xs

handleOption :: String -> String
handleOption ('-' : xs) = handleLongOption xs
handleOption "h"        = throw Help
handleOption xs         = throw $ InvalidOption xs

handleLongOption :: String -> String
handleLongOption "help" = throw Help
handleLongOption xs     = throw $ InvalidOption xs
