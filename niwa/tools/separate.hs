import System.Environment
import System.Directory
import System.FilePath

main :: IO ()
main = do
	[fn, dn] <- getArgs
	cont <- readFile fn
	let sep = separate [] cont
	createDirectory dn
	mapM_ (uncurry $ writeSeps dn) sep

writeSeps :: FilePath -> Int -> String -> IO ()
writeSeps fp n cnt = writeFile (joinPath [fp, show2 n ++ ".txt"]) $ show n ++ reverse cnt

show2 :: Int -> String
show2 n
	| n >= 0 && n < 10 = '0' : show n
	| otherwise = show n

separate :: [(Int, String)] -> String -> [(Int, String)]
separate r "" = r
separate r ('%':'s':'e':'c':'t':'i':'o':'n':':':body) =
	let (n, b) = span (/= '\n') body in
		separate ((read n, "") : r) b
separate ((n, s) : r) (c : cs) = separate ((n, c : s) : r) cs
