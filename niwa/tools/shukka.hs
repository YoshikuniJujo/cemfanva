import System.Environment
import Control.Applicative
import System.FilePath
import Data.Char
import Data.Function
import Data.List

main :: IO ()
main = do
	[dn, fl] <- getArgs
	fns <- lines <$> readFile (joinPath [dn, fl])
	conts <- mapM (readFile . joinPath . (dn :) . (: [])) fns
	putStr $ concat $ map
		(unlines . seikeiJE 80 . jeGroups .
			deleteMojiWords . deleteMyCom . deleteBrs . deleteComments) conts
--	putStr $ concat $ map (deleteMojiWords . deleteMyCom . deleteBrs . deleteComments) conts
--	putStr $ concat $ map (deleteBrs . deleteComments) conts

testData :: IO String
testData = readFile "fanva/01.txt"

data JE = Japanese | English deriving (Show, Eq)

seikeiJE :: Int -> [(JE, [String])] -> [String]
seikeiJE n jes = concatMap (seikeiJE1 n) jes

dropHeadSpace :: String -> String
dropHeadSpace (' ' : cs) = cs
dropHeadSpace cs = cs

seikeiJE1 :: Int -> (JE, [String]) -> [String]
seikeiJE1 n (Japanese, str) = take 4 str' ++
	reverse (map reverse $ seikeiJ n [] $ concat $ drop 4 str') ++ [""]
	where
	str' = map dropHeadSpace str
seikeiJE1 n (English, str) = unwords niho : "" :
	map unwords (reverse $ map reverse $ seikeiE n [] lihu) ++
	map unwords (reverse $ map reverse $ seikeiE n [] bi) ++ [""] ++
	map unwords (reverse $ map reverse $ seikeiE n [] body) ++ [""] ++
	map unwords (reverse $ map reverse $ seikeiE n [] tohi) ++ [""]
	where
	(body, tohi) = spanTOhISAhA ws'''
	(bi, ws''') = spanI ws''
	(lihu, ws'') = spanLIhU ws'
	(niho, ws') = spanMOhO ws
	ws = concatMap words str

spanI :: [String] -> ([String], [String])
spanI [] = ([], [])
spanI (".i" : ws) = ([], ".i" : ws)
spanI (w : ws) = let (bi, body) = spanI ws in (w : bi, body)

spanMOhO :: [String] -> ([String], [String])
spanMOhO [] = ([], [])
spanMOhO ("mo'o" : ws) = (["mo'o"], ws)
spanMOhO (w : ws) = let (niho, body) = spanMOhO ws in (w : niho, body)

spanLIhU :: [String] -> ([String], [String])
spanLIhU [] = ([], [])
spanLIhU ("li'u" : ws) = (["li'u"], ws)
spanLIhU (w : ws) = let (niho, body) = spanLIhU ws in (w : niho, body)

spanTOhISAhA :: [String] -> ([String], [String])
spanTOhISAhA [] = ([], [])
spanTOhISAhA ("to'i" : "sa'a" : ws) = ([], "to'i" : "sa'a" : ws)
spanTOhISAhA (w : ws) = let (body, tohi) = spanTOhISAhA ws in (w : body, tohi)

seikeiJ :: Int -> [String] -> String -> [String]
seikeiJ _ r "" = r
seikeiJ n [] (c : cs) = seikeiJ n [[c]] cs
seikeiJ n r@(h : t) (c : cs)
	| myLength h < n = seikeiJ n ((c : h) : t) cs
	| otherwise = seikeiJ n ([c] : r) cs

seikeiE :: Int -> [[String]] -> [String] -> [[String]]
seikeiE _ r [] = r
seikeiE n [] (w : ws) = seikeiE n [[w]] ws
seikeiE n r@(h : t) (w : ws)
	| sum (map ((+ 1) . length) h) + length w > n = seikeiE n ([w] : r) ws
	| otherwise = seikeiE n ((w : h) : t) ws

myLength :: String -> Int
myLength "" = 0
myLength (c : cs)
	| c `elem` japaneseChar = 2 + myLength cs
	| c `elem` englishChar = 1 + myLength cs
	| c `elem` "、。" = 2 + myLength cs
	| c `elem` " " = 1 + myLength cs
	| otherwise = error $ "bad: myLength " ++ [c]

jeGroups :: String -> [(JE, [String])]
jeGroups str = let (j, e) = sepNIhO str in [(Japanese, lines j), (English, lines e)]
{-
jeGroups = map (\ls -> (fst $ ls !! 0, map snd ls)) .
	groupBy (on (==) fst) . map addJE . lines
-}

sepNIhO :: String -> (String, String)
sepNIhO "" = ("", "")
sepNIhO ('n' : 'i' : '\'' : 'o' : cs) = ("", "ni'o" ++ cs)
sepNIhO (c : cs) = let (j, e) = sepNIhO cs in (c : j, e)

addJE :: String -> (JE, String)
addJE str
	| any (`elem` japaneseChar) str = (Japanese, str)
	| otherwise = (English, str)

englishChar, japaneseChar :: String
englishChar = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++
	".,'[]:*()/+-|?<>\"=\\_!$"
japaneseChar = ['あ' .. 'ん'] ++ ['ァ' .. 'ヴ'] ++ ['一' .. '黙'] ++
	"ー「」『』・々"

deleteComments :: String -> String
deleteComments "" = ""
deleteComments ('-' : '-' : '-' : 'c' : 'o' : '\'': 'a' : ' ' :
	'p' : 'i' : 'n': 'k' : 'a': '-' : '-' : '-' : _) = ""
deleteComments (c : cs) = c : deleteComments cs

deleteBrs :: String -> String
deleteBrs "" = ""
deleteBrs ('[' : cs) = deleteBrs $ dropWhile (/= ']') cs
deleteBrs (']' : cs) = deleteBrs cs
deleteBrs (c : cs) = c : deleteBrs cs

deleteMyCom :: String -> String
deleteMyCom "" = ""
deleteMyCom ('(' : '*' : cs) = case dropWhile isDigit cs of
	')' : cs' -> deleteMyCom cs'
	_ -> error "bad"
deleteMyCom ('(' : '2' : '0' : '1' : '2' : ')' : cs) = "(2012)" ++ deleteMyCom cs
deleteMyCom ('(' : '2' : '0' : '0' : '4' : ')' : cs) = "(2004)" ++ deleteMyCom cs
deleteMyCom ('(' : '2' : '0' : '0' : '7' : ')' : cs) = "(2007)" ++ deleteMyCom cs
deleteMyCom ('(' : cs) = case dropWhile isDigit cs of
	')' : cs' -> error $ takeWhile isDigit cs
	_ -> '(' : deleteMyCom cs
deleteMyCom (c : cs) = c : deleteMyCom cs

deleteMojiWords :: String -> String
deleteMojiWords "" = ""
deleteMojiWords (c : cs)
	| isDigit c = case dropWhile isDigit cs of
		'文' : '字' : '/' : cs' -> case dropWhile isDigit cs' of
			'w' : 'o' : 'r' : 'd' : 's' : cs'' -> cs''
			_ -> error "bad nan moji"
		'文' : '字' : cs' -> cs'
		'w' : 'o' : 'r' : 'd' : 's' : cs' -> cs'
		_ -> c : deleteMojiWords cs
	| otherwise = c : deleteMojiWords cs
