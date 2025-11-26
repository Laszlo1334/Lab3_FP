module TextProc
  ( normalizeSpaces
  , longestPalSubstring
  , allPalSubstrings
  ) where

import Data.List (nub)

-- Basic type aliases for readability
type Symbol      = Char
type WordT       = String
type Sentence    = String
type Punctuation = Char

-- Replace all runs of whitespace with a single space.
-- This uses words/unwords, which split on any whitespace.
normalizeSpaces :: Sentence -> Sentence
normalizeSpaces = unwords . words

-- Expand a palindrome around a given center (l, r),
-- returning the bounds [i, j) of the maximal palindromic substring.
expand :: Sentence -> Int -> Int -> (Int, Int)
expand s l r =
  let n = length s
      go i j
        | i < 0 || j >= n    = (i + 1, j)
        | s !! i /= s !! j   = (i + 1, j)
        | otherwise          = go (i - 1) (j + 1)
  in go l r

-- Find the longest palindromic substring in the normalized text.
longestPalSubstring :: Sentence -> Sentence
longestPalSubstring text =
  let s = normalizeSpaces text
      n = length s
  in if n == 0
       then ""
       else
         let indices = [0 .. n - 1]
             palAround i =
               let (l1, r1) = expand s i i       -- odd length
                   (l2, r2) = expand s i (i + 1) -- even length
               in [(l1, r1), (l2, r2)]
             allPals = concatMap palAround indices
             (bestL, bestR) =
               foldr
                 (\(l, r) best@(bl, br) ->
                    if r - l > br - bl then (l, r) else best)
                 (0, 1)
                 allPals
         in take (bestR - bestL) (drop bestL s)

-- Helper: check if a string is a palindrome as is
-- (including spaces and punctuation, case-sensitive).
isPalindrome :: Sentence -> Bool
isPalindrome s = s == reverse s

-- Find all palindromic substrings (length >= 2) in the normalized text.
-- This is an O(n^2) enumeration, which is fine for lab-sized inputs.
allPalSubstrings :: Sentence -> [Sentence]
allPalSubstrings text =
  let s   = normalizeSpaces text
      n   = length s
      subs = [ take (j - i) (drop i s)
             | i <- [0 .. n - 1]
             , j <- [i + 1 .. n]
             ]
  in nub
       [ sub
       | sub <- subs
       , length sub >= 2
       , isPalindrome sub
       ]
