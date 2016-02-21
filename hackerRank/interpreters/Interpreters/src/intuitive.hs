module Intuitive where

import Control.Monad
import qualified Data.Map                                       as M
import System.IO
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token            as Token

type VariableName = String

data Value = NConst Integer
           | 
