import System.Environment
import System.IO
import Data.List

type Memory = [Int]
type Cell = Int
type ParameterMode = Int


--splitAt :: Int -> [a] -> ([a], [a])
--splitAt _ [] = ([], [])
--splitAt 0 xs = ([], xs)
--splitAt i (x:xs) = (front ++ x, back)
--	where (front, back) = splitAt (i-1) xs

get :: Int -> Memory -> Cell
get index memory = memory !! index

set :: Int -> Cell -> Memory -> Memory
set index value memory = front ++ [value] ++ tail back
	where (front, back) = splitAt index memory

getParameter :: ParameterMode -> Int -> Memory -> Cell
getParameter 0 index memory = get (get index memory) memory
getParameter 1 index memory = get index memory

setParameter :: ParameterMode -> Int -> Cell -> Memory -> Memory
setParameter 0 index value memory = set (get index memory) value memory
setParameter 1 index value memory = set index value memory

b2i :: Bool -> Int
b2i True = 1
b2i False = 0

step :: Memory -> Int -> IO (Memory, Int)
step memory index
	| opcode == 99 = return (memory, i)
	| opcode == 1 = return (setParameter mode3 (i+3) ((getParameter mode1 (i+1) memory) + (getParameter mode2 (i+2) memory)) memory, (i+4))
	| opcode == 2 = return (setParameter mode3 (i+3) ((getParameter mode1 (i+1) memory) * (getParameter mode2 (i+2) memory)) memory, (i+4))
	| opcode == 3 = do
		input <- getLine
		let input_value = (read input) :: Int
		return (setParameter mode1 (i+1) input_value memory, i + 2)
	| opcode == 4 = do
		print $ getParameter mode1 (i+1) memory
		return (memory, i + 2)
	| opcode == 5 = return (if 0 /= getParameter mode1 (i+1) memory
		then (memory, getParameter mode2 (i+2) memory)
		else (memory, i + 3))
	| opcode == 6 = return (if 0 == getParameter mode1 (i+1) memory
		then (memory, getParameter mode2 (i+2) memory)
		else (memory, i + 3))
	| opcode == 7 = return (setParameter mode3 (i+3) (b2i ((getParameter mode1 (i+1) memory) < (getParameter mode2 (i+2) memory))) memory, (i+4))
	| opcode == 8 = return (setParameter mode3 (i+3) (b2i ((getParameter mode1 (i+1) memory) == (getParameter mode2 (i+2) memory))) memory, (i+4))
	| otherwise = return $ error $ "unrecognized opcode " ++ show opcode
	where
		i = index
		instruction = get index memory
		opcode = mod instruction 100
		mode1 = div (mod instruction 1000) 100
		mode2 = div (mod instruction 10000) 1000
		mode3 = div (mod instruction 100000) 10000


execute :: Memory -> Int -> IO Memory
execute memory index = do
	(nextmemory, nextindex) <- step memory index
	if index == nextindex
		then return memory
		else execute nextmemory nextindex


-- https://stackoverflow.com/a/4981265/5142683
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p s =
	case dropWhile p s of
		[] -> []
		s' -> w : splitBy p s''
			where (w, s'') = break p s'

parseMemoryChange :: String -> (Int, Int)
parseMemoryChange s = (read first, read last)
	where [first,last] = splitBy (== '=') s

applyChange :: Memory -> (Int, Int) -> Memory
applyChange memory (index,value) = set index value memory

applyChanges :: Memory -> [(Int, Int)] -> Memory
applyChanges memory [] = memory
applyChanges memory (x:xs) = applyChanges (applyChange memory x) xs

doAll :: [IO ()] -> IO ()
doAll [] = return ()
doAll (x:xs) = do x; doAll xs

isValid :: Memory -> Cell -> IO Bool
isValid memory requestedResult = do
	memory' <- execute memory 0
	return $ memory' !! 0 == requestedResult

combine :: IO a -> b -> c -> IO (a, b, c)
combine ia b c = do
	a <- ia
	return (a, b, c)

findValid :: Memory -> [Int] -> [Cell] -> Cell -> IO (Maybe [(Int, Cell)])
--findValid memory changeLocations values requestedResult
findValid memory [] _ requestedResult = do
	valid <- isValid memory requestedResult
	case valid of
		True -> return $ Just []
		False -> return $ Nothing
findValid memory (loc:locs) vals requestedResult = do
	rest <- sequence $ fmap (\value -> combine (findValid (applyChange memory (loc, value)) locs vals requestedResult) loc value) vals
	let valids = filter (\(valid,_,_) -> (case valid of Nothing -> False; Just _ -> True)) rest
	case valids of
		((Just changes, location, value):_) -> return $ Just ((location,value) : changes)
		[] -> return $ Nothing

main = do
	allArgs <- getArgs
	let memoryFilename = head allArgs
	let args = tail allArgs

	memoryFile <- openFile memoryFilename ReadMode
	rawInput <- hGetContents memoryFile
	let memory = map (read :: (String -> Int)) (splitBy (==',') (filter (/= '\n') rawInput))
	print memory
	putStrLn ""

	let (changesStrs, findStrs) = break (== "find") args
	let changes = map parseMemoryChange changesStrs
	let memory' = applyChanges memory changes

	case findStrs of
		[] -> do
			memory'' <- execute memory' 0
			print memory''
		"find":requestedResultStr:changeLocationsStrs -> do
			let requestedResult = (read requestedResultStr) :: Int
			let changeLocations = (map read changeLocationsStrs) :: [Int]
			putStr "Finding values at indices "
			let putInt = (\d -> putStr $ (show d) ++ ", ")
			doAll $ map putInt changeLocations
			putChar '\n'
			putStr "Such that memory' !! 0 == "
			putInt requestedResult
			putChar '\n'
			putChar '\n'

			let values = [0..99]
			valid <- findValid memory changeLocations values requestedResult
			print valid
