-- 単語の途中で切らないようにする
-- 。や、が頭に来ないようにする

-- とりあえず作ってみる
-- まずは test data を作成しよう

import Data.Char

main :: IO ()
main = interact seikei

sq :: Char
sq = chr 8217

seikei :: String -> String
seikei = unlines . concatMap (seikeiLine 70) . lines

eighty :: String -> [String]
eighty "" = []
eighty str = take 80 str : eighty (drop 80 str)

seikeiLine :: Int -> String -> [String]
seikeiLine n = reverse . map (myUnwords . reverse) . lessThans n . myWords

lessThans :: Int -> [String] -> [[String]]
lessThans n = flip foldl [] $ \r w ->
	case r of
		h : t -> if sum (map myLength' h) + myLength' w > n
			then [w] : r
			else (w : h) : t
		_ -> [[w]]

myLength' :: String -> Int
myLength' w
	| all (`elem` englishChar) w = myLength w + 1
	| otherwise = myLength w

myLength :: String -> Int
myLength "" = 0
myLength ('、' : cs) = myLength cs
myLength ('。' : cs) = myLength cs
myLength (c : cs)
	| c `elem` englishChar = 1 + myLength cs
	| c `elem` japaneseChar = 2 + myLength cs
	| c == '\8216' = 1 + myLength cs
	| c == japaneseCParens = 2 + myLength cs
	| c == japaneseQuestion = 2 + myLength cs
	| otherwise = error $ "bad char: " ++ show c

englishChar, japaneseChar :: String
englishChar = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++
	".,'[]:*()/+-|?<>\"=\\_!$"
japaneseChar = ['あ' .. 'ん'] ++ ['ァ' .. 'ヴ'] ++ ['一' .. '黙'] ++
	"ー「」『』・々"
japaneseDigit = ['\65296' .. '\65305']
japaneseOParens = '\65288'
japaneseCParens = '\65289'
japaneseBar = '\65372'
japaneseQuestion = '\65311'
japaneseSlash = '\65295'
japaneseColon = '\65306'
japaneseEqual = '\65309'
doubleOQuote = '\8220'
doubleCQuote = '\8221'
tripleDot = '\8230'
kai = '\215'

myUnwords :: [String] -> String
myUnwords [] = ""
myUnwords (w : ws)
	| all (`elem` englishChar) w = w ++ " " ++ myUnwords ws
	| all (`elem` (japaneseChar ++ "。、")) w = w ++ myUnwords ws
	| all (`elem` (englishChar ++ japaneseChar ++ "。、")) w = w ++ myUnwords ws
	| otherwise = error $ "myUnwords: bad " ++ show w

myWords :: String -> [String]
myWords "" = []
myWords ('\65279' : cs) = myWords cs
myWords ('\8216' : cs) = "\\8216" : myWords cs
myWords (c : '、' : cs) = [c, '、'] : myWords cs
myWords ('\65289' : '。' : cs) = [')', '。'] : myWords cs
myWords ('\65311' : '。' : cs) = ['?', '。'] : myWords cs
myWords (c : '。' : cs) = [c, '。'] : myWords cs
myWords (c : cs)
	| isSpace c = [] : myWords (dropWhile isSpace cs)
	| c == sq = ('\'' : h) : t
	| c == japaneseOParens = ('(' : h) : t
	| c == japaneseCParens = ")" : a
	| c == japaneseBar = "|" : a
	| c == japaneseQuestion = "?" : a
	| c == japaneseSlash = "/" : a
	| c == japaneseColon = ":" : a
	| c == japaneseEqual = "=" : a
	| c == '\65293' = "-" : a
	| c == tripleDot = "..." : a
	| c == doubleOQuote = "\"" : a
	| c == doubleCQuote = "\"" : a
	| c == kai = "x" : a
	| c `elem` englishChar = (c : h) : t
	| c `elem` japaneseChar = [c] : a
	| c `elem` japaneseDigit = [chr $ ord c - 65248] : a
	| otherwise = error $ "myWords: bad " ++ show c
	where
	a@(h : t) = myWords cs
