import System.Environment
import System.IO
import Data.List
import Text.Read (readMaybe)
import Data.Maybe (isJust)
import Data.IORef
import qualified Control.Monad.State as State

data ProgramState = Execute | Halt | IWait | OWait
	deriving (Show, Eq)
type Memory = [Int]
type Cell = Int
type ParameterMode = Int
type IOHandler monad = (monad (Maybe Int), Int -> monad (Maybe ()))
--data IOHandler = IOHandler {
--	ioin :: IO (Int, IOHandler),
--	ioout :: Int -> IO (IOHandler)
--}


--splitAt :: Int -> [a] -> ([a], [a])
--splitAt _ [] = ([], [])
--splitAt 0 xs = ([], xs)
--splitAt i (x:xs) = (front ++ x, back)
--	where (front, back) = splitAt (i-1) xs

get :: Int -> [a] -> a
get index memory = memory !! index

set :: Int -> a -> [a] -> [a]
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

step :: Monad m => Memory -> Int -> IOHandler m -> m (Memory, Int, IOHandler m, ProgramState)
step memory index io@(ioin, ioout)
	| opcode == 99 = return (memory, i, io, Halt)
	| opcode == 1 = return (setParameter mode3 (i+3) ((getParameter mode1 (i+1) memory) + (getParameter mode2 (i+2) memory)) memory, (i+4), io, Execute)
	| opcode == 2 = return (setParameter mode3 (i+3) ((getParameter mode1 (i+1) memory) * (getParameter mode2 (i+2) memory)) memory, (i+4), io, Execute)
	| opcode == 3 = do
		input <- ioin
		case input of
			Just value -> return (setParameter mode1 (i+1) value memory, i + 2, io, Execute)
			Nothing -> return (memory, i, io, IWait)
	| opcode == 4 = do
		output <- ioout $ getParameter mode1 (i+1) memory
		case output of
			Just _ -> return (memory, i + 2, io, Execute)
			Nothing -> return (memory, i, io, OWait)
	| opcode == 5 = return (if 0 /= getParameter mode1 (i+1) memory
		then (memory, getParameter mode2 (i+2) memory, io, Execute)
		else (memory, i + 3, io, Execute))
	| opcode == 6 = return (if 0 == getParameter mode1 (i+1) memory
		then (memory, getParameter mode2 (i+2) memory, io, Execute)
		else (memory, i + 3, io, Execute))
	| opcode == 7 = return (setParameter mode3 (i+3) (b2i ((getParameter mode1 (i+1) memory) < (getParameter mode2 (i+2) memory))) memory, (i+4), io, Execute)
	| opcode == 8 = return (setParameter mode3 (i+3) (b2i ((getParameter mode1 (i+1) memory) == (getParameter mode2 (i+2) memory))) memory, (i+4), io, Execute)
	| otherwise = return $ error $ "unrecognized opcode " ++ show opcode
	where
		i = index
		instruction = get index memory
		opcode = mod instruction 100
		mode1 = div (mod instruction 1000) 100
		mode2 = div (mod instruction 10000) 1000
		mode3 = div (mod instruction 100000) 10000


execute :: Monad m => Memory -> Int -> IOHandler m -> m Memory
execute memory index io = do
	(nextmemory, nextindex, nextio, nextstate) <- step memory index io
	case nextstate of
		Halt -> return memory
		Execute -> execute nextmemory nextindex nextio
		_ -> return $ error "Waiting on IO in execute"


-- https://stackoverflow.com/a/4981265/5142683
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p s =
	case dropWhile p s of
		[] -> []
		s' -> w : splitBy p s''
			where (w, s'') = break p s'

parseMemoryChange :: String -> Maybe (Int, Int)
parseMemoryChange s =
	case map (readMaybe :: String -> Maybe Int) $ splitBy (== '=') s of
		[Just first, Just last] -> Just (first, last)
		_ -> Nothing

applyChange :: Memory -> (Int, Int) -> Memory
applyChange memory (index,value) = set index value memory

applyChanges :: Memory -> [(Int, Int)] -> Memory
applyChanges memory [] = memory
applyChanges memory (x:xs) = applyChanges (applyChange memory x) xs

doAll :: [IO ()] -> IO ()
doAll [] = return ()
doAll (x:xs) = do x; doAll xs

isValid :: Monad m => Memory -> Cell -> IOHandler m -> m Bool
isValid memory requestedResult io = do
	memory' <- execute memory 0 io
	return $ memory' !! 0 == requestedResult

combine :: Monad m => m a -> b -> c -> m (a, b, c)
combine ma b c = do
	a <- ma
	return (a, b, c)

findValid :: Monad m => Memory -> [Int] -> [Cell] -> Cell -> IOHandler m -> m (Maybe [(Int, Cell)])
--findValid memory changeLocations values requestedResult io
findValid memory [] _ requestedResult io = do
	valid <- isValid memory requestedResult io
	case valid of
		True -> return $ Just []
		False -> return $ Nothing
findValid memory (loc:locs) vals requestedResult io = do
	rest <- sequence $ fmap (\value -> combine (findValid (applyChange memory (loc, value)) locs vals requestedResult io) loc value) vals
	let valids = filter (\(valid,_,_) -> (case valid of Nothing -> False; Just _ -> True)) rest
	case valids of
		((Just changes, location, value):_) -> return $ Just ((location,value) : changes)
		[] -> return $ Nothing

myPartition :: (a -> Maybe b) -> [a] -> ([b], [a])
myPartition _ [] = ([],[])
myPartition f (x:xs) =
	case f x of
		Just b -> (b:bs, as)
		Nothing -> (bs, x:as)
	where (bs, as) = myPartition f xs

getNextInput :: State.StateT ([Cell], [Cell]) IO (Maybe Cell)
getNextInput = do
	(xs, ys) <- State.get
	case xs of
		[] -> return Nothing
		(x:xs) -> do
			State.put (xs,ys)
			return $ Just x
	
putNextOutput :: Cell -> State.StateT ([Cell], [Cell]) IO (Maybe ())
putNextOutput y = do
	(xs, ys) <- State.get
	State.put (xs,ys ++ [y])
	return $ Just ()
	
range :: Int -> [Int]
range 0 = []
range x = (range (x-1)) ++ [x-1]

-- returns the final output
chainExecutions :: Memory -> [Cell] -> Cell -> IO Cell
chainExecutions memory [] start_value = return start_value
chainExecutions memory (c:cs) start_value = do
	let io = (getNextInput, putNextOutput)
	let state = ([c,start_value], [])
	(memory', state') <- State.runStateT (execute memory 0 io) state
	chainExecutions memory cs $ head $ snd state'


getLoopedInput :: Int -> State.StateT [[Cell]] IO (Maybe Cell)
getLoopedInput index = do
	xss <- State.get
	let xs = get index xss
	case xs of
		[] -> return Nothing
		(x:xs) -> do
			State.put $ set index xs xss
			return $ Just x
	
putLoopedOutput :: Int -> Cell -> State.StateT [[Cell]] IO (Maybe ())
putLoopedOutput index y = do
	xss <- State.get
	let xs = get index xss
	State.put $ set index (xs ++ [y]) xss
	return $ Just ()

main = do
	allArgs <- getArgs
	let memoryFilename = head allArgs
	let args = tail allArgs

	memoryFile <- openFile memoryFilename ReadMode
	rawInput <- hGetContents memoryFile
	let originalMemory = map (read :: (String -> Int)) (splitBy (==',') (filter (/= '\n') rawInput))
	print originalMemory
	putStrLn ""

	let (changes, args') = myPartition parseMemoryChange args
	let memory = applyChanges originalMemory changes

	let defaultio = (fmap (readMaybe :: String -> Maybe Int) getLine, (\d -> do dd <- print d; return $ Just dd)) :: IOHandler IO

	case args' of
		[] -> do
			memory' <- execute memory 0 defaultio
			print memory'
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
			valid <- findValid memory changeLocations values requestedResult defaultio
			print valid
		"sequence":countStr:[] -> do
			let count = read countStr :: Int
			let configurations = permutations $ range count
			let try_configuration = \config -> chainExecutions memory config 0
			
			all_outputs <- sequence $ fmap (\config -> combine (try_configuration config) config ()) configurations
			
			let (best_output,best_config,_) = foldl (\b@(b1,_,_) -> \a@(a1,_,_) -> if a1 > b1 then a else b) (minBound :: Int, [], ()) all_outputs
			print best_output
		_ -> do
			print $ permutations args'
