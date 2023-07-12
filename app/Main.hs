{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import GameOfLife
import System.Console.ANSI
import System.Environment

main :: IO ()
main = do
  hideCursor
  dimensions <- getTerminalSize
  args <- getArgs
  (rows, cols) <-
    if length args >= 3
      then do
        let [_, rows, cols] = take 3 args
        return (read rows, ((read cols) :: Int) * 2)
      else case dimensions of
        Just dim -> return dim
        Nothing -> return (27, 54)
  let (r3 ::Int) = (div rows  3) * 3 - 3 -- subtracting 3 for maintaining border
      (c3 :: Int) = (div cols 6) * 3
  startLife (getRule args) r3 c3
 where
  getRule args =
    if not (null args)
      then read $ head args
      else -1
