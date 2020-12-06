slice :: Int -> Int -> [a] -> ([a], [a], [a])
slice 0 0 rest = ([], [], rest)
slice 0 stop (x:xs) =
	let (p, q, r) = slice 0 (stop-1) xs
	in (p, x:q, r)
slice start stop (x:xs) =
	let (p, q, r) = slice (start-1) (stop-1) xs
	in (x:p, q, r)

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks c x =
	let (p, q, r) = slice 0 c x
	in q : chunks c r

count :: Eq a => a -> [a] -> Int
count x [] = 0
count x (y:ys)
	| (x == y) = 1 + count x ys
	| otherwise = count x ys

indexOf :: Eq a => a -> [a] -> Maybe Int
indexOf _ [] = Nothing
indexOf x (y:ys)
	| (x == y) = Just 0
	| otherwise = fmap (+1) (indexOf x ys)

pixelColor :: Int -> [[Char]] -> Char
pixelColor _ [] = 'X'
pixelColor i (layer:layers)
	| (c == '2') = pixelColor i layers
	| otherwise = c
	where c = layer !! i

prettyChar :: Char -> [Char]
prettyChar '0' = " "
prettyChar '1' = "\x1b[107m \x1b[0m"
prettyChar _ = "\x1b[41mX\x1b[0m"

prettyImage :: [Char] -> [Char]
prettyImage [] = []
prettyImage (c:cs) = (prettyChar c) ++ prettyImage cs

main = do
	input <- getContents
	let image = head $ lines input
	let layers = chunks (25 * 6) image
	let zcount = fmap (count '0') layers
	let minIndex = (\(Just x) -> x) (indexOf (minimum zcount) zcount)

	putStr "Part 1: "
	print minIndex

	let layer = layers !! minIndex
	let actualImage = fmap (\i -> pixelColor i layers) [0, 1 .. (25*6-1)]

	putStrLn "Part 2: "
	mapM_ putStrLn $ fmap prettyImage $ chunks 25 actualImage
