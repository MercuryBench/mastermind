import qualified Data.Set as Set  
import qualified Data.List as List
import Control.Monad
import System.Random

data Colour = R | G | B | Y | T | P | W | O
	deriving (Show, Eq, Ord)
type Redflags = Int
type Whiteflags = Int
type Setting = [Colour]
type Pattern = [Colour]

data Feedback = Feedback Setting Redflags Whiteflags
	deriving (Show)

redflags p s = (p', s', n) where
	remaining = filter (\(x, y) -> (x /= y)) (zip p s)
	p' = map fst remaining
	s' = map snd remaining
	n = length p - length remaining

deleteFirstOcc y (x:xs) = if y == x then xs else x: deleteFirstOcc y xs

whiteflags_rec ([], _, n) = ([], [], n)
whiteflags_rec (_, [], n) = ([], [], n)
whiteflags_rec (xs, y:ys, n) 
	| ind == Nothing = whiteflags_rec (xs, ys, n)
	| otherwise = whiteflags_rec (deleteFirstOcc y xs, ys, n+1)
--	| Set.member y (Set.fromList xs) = whiteflags_rec (Set.toList (Set.delete y (Set.fromList xs)), ys, n+1)
--	| otherwise = whiteflags_rec (xs, ys, n)
	where ind = List.elemIndex y xs

whiteflags p s = n where
	(_, _, n) = whiteflags_rec (p, s, 0)

comp :: Pattern -> Setting -> (Redflags, Whiteflags)
comp p s = (m, n) where
	(p', s', m) = redflags p s
	n = whiteflags p' s'
	
filterFromResult :: Pattern -> (Redflags, Whiteflags) -> Pattern -> Bool
filterFromResult p (m, n) p' = comp p p' == (m, n)

countToFreq :: (Integral a) => [a] -> [Double]
countToFreq xs = map ((/ (fromIntegral (sum (xs)))). fromIntegral) xs

entropy :: [Double] -> Double
entropy [] = 0
entropy (x:xs) = -(x * log x) + entropy xs

--getFreqMaps :: [Pattern] -> [[Double]]
getFreqMaps [] = [[]]
getFreqMaps xs = [map (\ys@(y:_) -> (y, length ys)) . List.group . List.sort $ map (comp (x)) xs| x<-xs]

--findFstWithMaxSnd :: [(a, b)] -> a
findFstWithMaxSnd (x:xs) = fst $ foldl (\acc x -> if snd x > snd acc then x else acc) x (x:xs)

checkResult :: Pattern -> IO (Redflags, Whiteflags)
checkResult guess = do
	putStrLn $ "How about " ++ show guess ++ "?"
	putStrLn "Num of red flags?"
	m' <- getLine 
	let m = read m' :: Int
	putStrLn "Num of white flags?"
	n' <- getLine
	let n = read n' :: Int
	return (m, n)

main = do
	gen <- getStdGen
	let l0 = [[s1, s2, s3, s4]| (s1,s2,s3,s4) <- liftM4 (,,,) [R,G,B,Y,T,P,W,O] [R,G,B,Y,T,P,W,O] [R,G,B,Y,T,P,W,O] [R,G,B,Y,T,P,W,O]]
	let (r, g) = randomR (0, length l0 - 1) gen
	step l0 (l0 !! r) g

step listOfPossibleSettings bestguess rgen = do
	(m, n) <- checkResult bestguess
	if (m, n) /= (4, 0) then do
		let l = filter ((==) (m, n) . comp bestguess) listOfPossibleSettings
		putStrLn $ show l
		if length l < 200 then do
			let l2 = getFreqMaps l
			let l3 = map (entropy . countToFreq) $ (map . map) snd l2
			let l4 = zip l l3
			let choice = findFstWithMaxSnd l4
			if length choice == 1 then
				return choice
			else
				step l choice rgen
		else do
			let (r, g) = randomR (0, length l - 1) rgen
			let choice = listOfPossibleSettings !! r
			step l choice g
	else do 
		putStrLn "I won!"
		return bestguess
-- l = filter ((==) (1,2) . comp [G, B, Y, B]) [[s1, s2, s3, s4]| (s1,s2,s3,s4) <- liftM4 (,,,) [R,G,B,Y,T,P,W,O] [R,G,B,Y,T,P,W,O] [R,G,B,Y,T,P,W,O] [R,G,B,Y,T,P,W,O]]
-- l2 = getFreqMaps l
-- l3 = map (entropy . countToFreq) $ (map . map) snd l2
-- l4 = zip l l3
-- choice = findFstWithMaxSnd l4

-- let freqmap = map (\xs@(x:_) -> (x, length xs)) . List.group . List.sort $ map (comp (head l)) l


--l1 = filter ((==) (1,2) . getFeedback [R, G, B, Y]) [[s1,s2,s3,s4]| s1<-[R,G,B,Y,T,P,W,O],s2<-[R,G,B,Y,T,P,W,O], s3<-[R,G,B,Y,T,P,W,O], s4<-[R,G,B,Y,T,P,W,O]]
--freqmap = map (\xs@(x:_) -> (x, length xs)) . group . sort $ map (getFeedback (head l1)) l1
--map snd freqmap
