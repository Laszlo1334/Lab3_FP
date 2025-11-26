import Test.Hspec
import TextProc

main :: IO ()
main = hspec $ do

  describe "normalizeSpaces" $ do
    it "collapses multiple spaces between words" $
      normalizeSpaces "a   b    c" `shouldBe` "a b c"

    it "handles tabs and newlines" $
      normalizeSpaces "a\tb\nc" `shouldBe` "a b c"

    it "trims leading and trailing spaces" $
      normalizeSpaces "   a b   " `shouldBe` "a b"

    it "returns empty string for only whitespace" $
      normalizeSpaces " \t  \n   " `shouldBe` ""

  describe "longestPalSubstring" $ do
    -- basic cases
    it "returns empty string for empty input" $
      longestPalSubstring "" `shouldBe` ""

    it "returns the only character for a length-1 string" $
      longestPalSubstring "x" `shouldBe` "x"

    it "returns some single character when no longer palindrome exists" $ do
      let r = longestPalSubstring "abc"
      r `elem` ["a","b","c"] `shouldBe` True

    -- simple word palindromes
    it "finds full odd-length palindrome word" $
      longestPalSubstring "racecar" `shouldBe` "racecar"

    it "finds full even-length palindrome word" $
      longestPalSubstring "abba" `shouldBe` "abba"

    -- palindromes inside longer strings
    it "finds inner palindrome substring" $
      longestPalSubstring "xyzabacdf" `shouldBe` "aba"

    -- spaces and normalization
    it "finds palindrome with spaces after normalization" $
      longestPalSubstring "a    b    a" `shouldBe` "a b a"

    -- punctuation
    it "handles punctuation inside and at the end of words" $ do
      longestPalSubstring "a,b,a" `shouldBe` "a,b,a"
      longestPalSubstring "civic," `shouldBe` "civic"
