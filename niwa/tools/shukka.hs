import System.Environment
import Control.Applicative
import System.FilePath
import Data.Char

main :: IO ()
main = do
	[dn, fl] <- getArgs
	fns <- lines <$> readFile (joinPath [dn, fl])
	conts <- mapM (readFile . joinPath . (dn :) . (: [])) fns
	putStr $ concat $ map (deleteMojiWords . deleteMyCom . deleteBrs . deleteComments) conts
--	putStr $ concat $ map (deleteBrs . deleteComments) conts

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
