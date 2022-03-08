--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Exception
--

module Exception     ( KoakException(..) ) where

import GHC.Exception ( Exception )

import qualified Argument.Parser.Exception as APE ( KoakArgumentParserException(..) )
import qualified Koak.Lexer.Exception      as KLE ( KoakLexerException(..) )
import qualified Koak.Parser.Exception     as KPE ( KoakParserException(..) )

data KoakException = KoakAPE APE.KoakArgumentParserException
                   | KoakKLE KLE.KoakLexerException
                   | KoakKPE KPE.KoakParserException
    deriving (Show, Eq)

instance Exception KoakException
