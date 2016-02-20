module Main where

import qualified Data.List                      as L
import qualified Data.Map                       as M
import While
import Text.ParserCombinators.Parsec

main = do
    cs <- getContents
    case parse whileParser "" cs of
        Left e -> print e >> fail "oh no"
        Right prog -> let
            vars = interpret prog initialState
            out = L.unlines . map (\(n,v)-> n ++ " " ++ (show v)) . M.toAscList $ vars
            in putStr out
