import Data.Char

delAllUpper :: String -> String
delAllUpper = unwords . filter (all isUpper) . words
