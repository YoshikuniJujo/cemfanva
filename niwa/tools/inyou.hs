import System.Environment
import Data.Char

main :: IO ()
main = do
	[b, n] <- fmap (map read) getArgs
	interact $ unlines . addInyou b n . lines

addInyou :: Int -> Int -> [String] -> [String]
addInyou b n str =
	take b str ++ map addIS (take n $ drop b str) ++ drop (b + n) str

addI :: String -> String
addI = unlines . map addIS . lines

addIS :: String -> String
addIS str
	| all isSpace str = ""
	| otherwise = "> " ++ str
