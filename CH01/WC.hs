main = interact wordCount
     where wordCount input = show (sum $ map length (lines input)) ++ "\n"
