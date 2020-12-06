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

main = do
	input <- getContents
	let image = head $ lines input
	let layers = chunks (25 * 6) image
	print $ length layers
	let zcount = fmap (count '0') layers
	print zcount
	let minIndex = (\(Just x) -> x) (indexOf (minimum zcount) zcount)
	print minIndex
	let layer = layers !! minIndex
	print $ layer
	print $ (count '1' layer) * (count '2' layer)

