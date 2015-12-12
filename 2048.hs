import Data.Char (intToDigit)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes, fromJust, maybeToList)
import Data.List (transpose, intercalate, maximumBy)
import Data.Ord (comparing)
import Control.Monad (liftM2)
import Control.Monad.Random


boardSize = 4


-- utility
(.>) = flip (.)
infixl 9 .>


-- merge pairs
collapse :: [Int] -> [Int]
collapse [] = []
collapse [x] = [x]
collapse (x:y:rest) | x==y       = (x+1) : collapse rest
                    | otherwise  = x : collapse (y:rest)

backfillNothing :: Int -> [a] -> [Maybe a]
backfillNothing 0 _ = []
backfillNothing n (x:xs) = Just x : backfillNothing (n-1) xs
backfillNothing n [] = replicate n Nothing

compressLine :: [Maybe Int] -> [Maybe Int]
compressLine = backfillNothing boardSize . collapse . catMaybes


data Board = Board [[Maybe Int]]
  deriving (Eq, Ord)

unBoard :: Board -> [[Maybe Int]]
unBoard (Board rows) = rows

instance Show Board where
  show = unBoard .> concatMap (\row -> map digit row ++ "|\n")

digit :: Maybe Int -> Char
digit Nothing = ' '
digit (Just n) = intToDigit n

emptyBoard :: Board
emptyBoard = Board $ replicate boardSize $ replicate boardSize Nothing


data Direction = L | R | U | D
  deriving (Eq, Ord, Show, Read)

dirs :: [Direction]
dirs = [L, R, U, D]


addRandomTile :: (MonadRandom m) => Board -> m Board
addRandomTile board = do
  position <- uniform (emptyPositions board)
  value <- fromList [(Just 0, 0.9), (Just 1, 0.1)]
  return $ setTile position value board

emptyPositions :: Board -> [(Int, Int)]
emptyPositions =
  unBoard .> zip [0..]
  .> map (\(i, row) -> zip (zip (repeat i) [0..]) row)
  .> concat .> filter (snd .> (==Nothing))
  .> map fst

lost :: Board -> Bool
lost b = null $ possibleMoves b

possibleMoves :: Board -> [Board]
possibleMoves b = filter (/= b) $ map (flip pushTiles b) dirs

setTile pos val =
  unBoard .> mapAt (fst pos) (mapAt (snd pos) (const val)) .> Board

mapAt :: Int -> (a -> a) -> [a] -> [a]
mapAt n f = go 0
  where go i [] = []
        go i (x:xs)  | i==n      = (f x):xs
                     | otherwise = x : go (i+1) xs

pushTiles :: Direction -> Board -> Board
pushTiles dir  =
  unBoard .> outer dir .> inner dir
  .> map compressLine
  .> inner dir .> outer dir .> Board
  where
    outer L = id
    outer R = id
    outer U = transpose
    outer D = transpose
    inner L = id
    inner R = map reverse
    inner U = id
    inner D = map reverse


allTiles :: Board -> [Int]
allTiles = unBoard .> concat .> catMaybes

highestTile :: Board -> Int
highestTile = allTiles .> (0:) .> maximum


type Strategy = Board -> Board -> Ordering

tileRating :: (Ord a, Num a) => (Int -> a) -> Strategy
tileRating f = comparing $ allTiles .> map f .> sum


minorNeighbourRating :: (Ord a, Num a) => (Int -> a) -> Strategy
minorNeighbourRating f = comparing $ minorNeighbours .> map f .> sum

minorNeighbours :: Board -> [Int]
minorNeighbours b =
  (minorNeighboursH $ unBoard b)
  ++ (minorNeighboursH $ transpose $ unBoard b)
  where minorNeighboursH = map minorNeighboursRow .> concat

minorNeighboursRow :: [Maybe Int] -> [Int]
minorNeighboursRow (x:xs) = maybeToList x ++ go (x:xs)
  where go :: [Maybe Int] -> [Int]
        go [x] = maybeToList x
        go (x:y:xs) = maybeToList (liftM2 min x y) ++ go (y:xs)


strategyTurn :: Strategy -> Board -> Maybe Board
strategyTurn s b =
  if not (null options)
    then Just $ maximumBy s options
    else Nothing  -- that means exactly (lost b)
  where options = possibleMoves b


strategyPlay :: MonadRandom m => Strategy -> m [Board]
strategyPlay s = strategyPlayBoard s emptyBoard

strategyPlayBoard :: MonadRandom m => Strategy -> Board -> m [Board]
strategyPlayBoard s board = do
  board' <- addRandomTile board
  case strategyTurn s board' of
    Nothing -> return [board']
    Just board'' -> do
      furtherGame <- strategyPlayBoard s board''
      return $ board' : furtherGame


userPlay :: IO Board
userPlay = userPlayBoard emptyBoard

userPlayBoard :: Board -> IO Board
userPlayBoard board = do
  board' <- evalRandIO $ addRandomTile board
  case lost board' of
    True  -> putStrLn "Game over. Not bad." >> return board'
    False -> askTurn board' >>= userPlayBoard

askTurn :: Board -> IO Board
askTurn board = do
  putStrLn $ replicate boardSize '-'
  putStr $ show board
  maybeDir <- fmap readMaybe getLine
  case maybeDir of
    Nothing  -> askTurn board
    Just dir ->
      let board' = pushTiles dir board in
      case board' /= board of
        False -> askTurn board
        True  -> return board'


-- output utility
oneAtATime :: Show a => [a] -> IO a
oneAtATime [] = return undefined
oneAtATime (x:xs) = do
  putStrLn $ show x
  line <- getLine
  case line of
    "" -> oneAtATime xs
    _  -> return x
