module Main where

import While
import Text.ParserCombinators.Parsec

main :: IO ()
main = do
    cs <- getContents
    case parse whileParser "" cs of
        Left e -> print e >> fail "parse error"
        Right r -> print r
