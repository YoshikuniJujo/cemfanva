
main :: IO ()
main = interact changeNL

changeNL :: String -> String
changeNL "" = ""
changeNL ('\r' : '\n' : cs) = '\n' : changeNL cs
changeNL (c : cs) = c : changeNL cs
