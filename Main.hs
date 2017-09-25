{-# OPTIONS_GHC -fwarn-missing-signatures #-}

import Control.Monad
import Data.Array
import Data.Char
import Data.Maybe
import Data.Tree
import System.IO
import System.IO.Unsafe
import Text.ParserCombinators.Parsec

data BrainBleepOp
  = Program
  | Increment
  | Decrement
  | ShiftLeft
  | ShiftRight
  | PutChar
  | GetChar
  | DoUntilZero
  deriving (Show, Read)

main = do
  x <- getContents
  interpretBrainBleep x

opDefs =
  [ ('+', Increment)
  , ('-', Decrement)
  , (',', GetChar)
  , ('.', PutChar)
  , ('<', ShiftLeft)
  , ('>', ShiftRight)
  ]

ops =
  oneOf (map fst opDefs) >>= \x -> return $ Node (fromJust $ lookup x opDefs) []

doUntilZero = do
  char '['
  (Node Program x) <- myParser
  char ']'
  return (Node DoUntilZero x)

myParser =
  sepBy (many (try doUntilZero <|> try ops)) newline >>=
  return . Node Program . concat

exampleAst src = parse myParser "" (filter (/= ' ') src)

initArray = listArray (0, 30000 - 1) (replicate 30000 0)

interpretBrainBleep src =
  runAst' (0, initArray) ((\(Right x) -> x) (exampleAst src)) >> return ()

runAst' (cursor, array) l@(Node DoUntilZero xs) =
  if array ! cursor == 0
    then return (cursor, array)
    else do
      s <- foldM runAst' (cursor, array) xs
      runAst' s l
runAst' (cursor, array) (Node Program xs) = foldM runAst' (cursor, array) xs
runAst' (cursor, array) (Node Increment _) =
  putStr "" >> return (cursor, array // [(cursor, array ! cursor + 1)])
runAst' (cursor, array) (Node Decrement _) =
  putStr "" >> return (cursor, array // [(cursor, array ! cursor - 1)])
runAst' (cursor, array) (Node ShiftLeft _) =
  putStr "" >> return (cursor - 1, array)
runAst' (cursor, array) (Node ShiftRight _) =
  putStr "" >> return (cursor + 1, array)
runAst' (cursor, array) (Node GetChar _) =
  getChar >>= \c -> return (cursor, array // [(cursor, ord c)])
runAst' (cursor, array) (Node PutChar _) =
  putChar (chr $ array ! cursor) >> return (cursor, array)
