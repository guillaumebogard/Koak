--
-- EPITECH PROJECT, 2022
-- KOAK
-- File description:
-- Main
--

module Main where

import System.Environment                         ( getArgs )
import System.Exit                                ( ExitCode(ExitFailure)
                                                  , exitWith
                                                  , exitSuccess
                                                  )
import Control.Exception                          ( Handler(..)
                                                  , catches
                                                  )

import qualified Argument.Parser.Exception as APE ( KoakArgumentParserException( KoakHelpException ) )
import qualified Koak.Lexer.Exception      as KLE ( KoakLexerException(..) )
import qualified Koak.Parser.Exception     as KPE ( KoakParserException(..) )
import qualified Koak.Typing.Exception     as KTE ( KoakTypingException(..) )

import qualified Argument.Parser           as AP  ( KoakArguments(..)
                                                  , Filepath(..)
                                                  , parseArguments
                                                  )
import qualified Koak.Parser               as KP  (parseKoak)
import qualified Koak.Typing        as KT
import qualified Koak.TypingContext as KTC

main :: IO ()
main = (getArgs >>= handleExecution . AP.parseArguments) `catches` [ Handler exceptionHandlerAPE
                                                                   , Handler exceptionHandlerKLE
                                                                   , Handler exceptionHandlerKPE
                                                                   , Handler exceptionHandlerKTE
                                                                   ]

handleExecution :: AP.KoakArguments -> IO ()
handleExecution (AP.KoakArguments (AP.Filepath file)) = readFile file >>= launchExecution

launchExecution :: String -> IO ()
launchExecution file = print (KT.checkKoakTyping (KP.parseKoak file) KTC.getDefaultKContext)

exceptionHandlerAPE :: APE.KoakArgumentParserException -> IO ()
exceptionHandlerAPE APE.KoakHelpException   = print APE.KoakHelpException >> exitSuccess
exceptionHandlerAPE exception               = print exception             >> exitWith (ExitFailure 84)

exceptionHandlerKLE :: KLE.KoakLexerException -> IO ()
exceptionHandlerKLE exception               = print exception             >> exitWith (ExitFailure 84)

exceptionHandlerKPE :: KPE.KoakParserException -> IO ()
exceptionHandlerKPE exception               = print exception             >> exitWith (ExitFailure 84)

exceptionHandlerKTE :: KTE.KoakTypingException -> IO ()
exceptionHandlerKTE exception               = print exception             >> exitWith (ExitFailure 84)
