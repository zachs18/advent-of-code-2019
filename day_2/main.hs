import System.Environment
import System.IO

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


-- This won't work (yet) because i don't completely get IO yet.
execute :: Memory -> Int -> IO Memory
--execute memory index
--	| index == nextindex = return memory
--	| otherwise = execute nextmemory nextindex
--	where (nextmemory, nextindex) = step memory index
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

main = do
	allArgs <- getArgs
	let memoryFilename = head allArgs
	let args = tail allArgs

	memoryFile <- openFile memoryFilename ReadMode
	rawInput <- hGetContents memoryFile
	let originalMemory = map (read :: (String -> Int)) (splitBy (==',') (filter (/= '\n') rawInput))
	print originalMemory
	putStrLn ""

	if (length args > 1) && (head args == "find")
		then do
			let requestedResult = (read $ head $ tail $ args) :: Int
			let args' = tail $ tail args
			let changeLocations = (map read args') :: [Int]
			putStr "Finding values at indices "
			let putInt = (\d -> putStr $ (show d) ++ ", ")
			doAll $ map putInt changeLocations
			putChar '\n'
			putStr "Such that memory' !! 0 == "
			putInt requestedResult
			putChar '\n'
		else do
			let changes = map parseMemoryChange args
			let memory = applyChanges originalMemory changes

			let input = map (read :: (String -> Int)) args

			-- print $ execute memory input [] 0
			putStrLn ""
			memory' <- execute memory 0
			print memory'
