module Main where

import TextProc (normalizeSpaces, longestPalSubstring, allPalSubstrings)

main :: IO ()
main = do
  content <- readFile "input.txt"

  let norm    = normalizeSpaces content
      longest = longestPalSubstring content
      pals    = allPalSubstrings content

  putStrLn "=== Normalized text ==="
  putStrLn norm

  putStrLn "\n=== Longest palindromic substring ==="
  if null longest
    then putStrLn "(no palindrome found)"
    else putStrLn longest

  putStrLn "\n=== All palindromic substrings (length >= 2) ==="
  if null pals
    then putStrLn "(no palindromes found)"
    else do
      putStrLn $ "Total: " ++ show (length pals)
      mapM_ (\p -> putStrLn ("- " ++ p)) pals
