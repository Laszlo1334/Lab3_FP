module TextProc
  ( normalizeSpaces
  , longestPalSubstring
  ) where

import Data.Char (isSpace)

-- Замінює таби/кілька пробілів одним пробілом
normalizeSpaces :: String -> String
normalizeSpaces = unwords . words

-- Допоміжна функція для розширення паліндрома навколо центру
expand :: String -> Int -> Int -> (Int, Int)
expand s l r =
  let n = length s
      go i j
        | i < 0 || j >= n      = (i + 1, j)
        | s !! i /= s !! j     = (i + 1, j)
        | otherwise            = go (i - 1) (j + 1)
  in go l r

-- Пошук найдовшого паліндромного підрядка в нормалізованому тексті
longestPalSubstring :: String -> String
longestPalSubstring text =
  let s = normalizeSpaces text
      n = length s
  in if n == 0
       then ""
       else
         let indices  = [0 .. n - 1]
             palAround i =
               let (l1, r1) = expand s i i       -- непарна довжина
                   (l2, r2) = expand s i (i+1)   -- парна довжина
               in [(l1, r1), (l2, r2)]
             allPals = concatMap palAround indices
             (bestL, bestR) =
               foldr
                 (\(l, r) best@(bl, br) ->
                    if r - l > br - bl then (l, r) else best)
                 (0, 1)
                 allPals
         in take (bestR - bestL) (drop bestL s)
