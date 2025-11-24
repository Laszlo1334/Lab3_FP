module Main where

import TextProc (normalizeSpaces, longestPalSubstring)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let norm = normalizeSpaces content
      pal  = longestPalSubstring content
  putStrLn "Normalized text:"
  putStrLn norm
  putStrLn "\nLongest palindromic substring:"
  putStrLn pal
