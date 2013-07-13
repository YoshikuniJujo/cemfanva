main :: IO ()
main = interact $ reverse . deleteSpace . reverse

deleteSpace :: String -> String
deleteSpace "" = ""
deleteSpace ('\n' : cs) = '\n' : deleteSpace (dropWhile (== ' ') cs)
deleteSpace (c : cs) = c : deleteSpace cs
