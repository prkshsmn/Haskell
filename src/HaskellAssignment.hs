module HaskellAssignment where

------------------------------------------------
-- findFirst
------------------------------------------------
data Found = Match Int | NoMatch deriving Eq
instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"
findFirst :: Eq a => (a -> Bool) -> [a] -> Found


findFirst needle haystack = findFirstHelper 0 haystack
  where
    findFirstHelper _ [] = NoMatch
    findFirstHelper index (x:xs)
      | needle x = Match index
      | otherwise = findFirstHelper (index + 1) xs
------------------------------------------------
-- palindrome
------------------------------------------------
palindrome :: [Char] -> Bool


palindrome s = s == reverse s