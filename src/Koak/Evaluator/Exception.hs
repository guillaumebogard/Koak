--
-- EPITECH PROJECT, 2022
-- koak
-- File description:
-- Koak.Parser.Exception
--

module Koak.Evaluator.Exception ( KoakEvaluatorException(..) ) where

import GHC.Exception            ( Exception )


newtype KoakEvaluatorException = KoakEvaluatorException String
    deriving Eq

instance Exception KoakEvaluatorException

instance Show      KoakEvaluatorException where
    show (KoakEvaluatorException str) = str
