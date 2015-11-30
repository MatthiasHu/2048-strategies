import Data.Char (intToDigit)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)
import Data.List (transpose, intercalate, maximumBy)
import Control.Monad.Random


boardSize = 4


-- utility
(.>) = flip (.)
infixl 9 .>


-- merge pairs
collapse :: [Int] -> [Int]
collapse [] = []
collapse [x] = [x]
collapse (x:y:rest) | x==y       = (x+1) : rest
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
lost b = null $ emptyPositions b
-- that's wrong. TODO: correct this.

setTile pos val =
  unBoard .> mapAt (fst pos) (mapAt (snd pos) (const val)) .> Board

mapAt :: Int -> (a -> a) -> [a] -> [a]
mapAt n f = go 0
  where go i [] = []
        go i (x:xs)  | i==n      = (f x):xs
                     | otherwise = x : go (i+1) xs


highestTile :: Board -> Int
highestTile = unBoard .> concat .> catMaybes .> (0:) .> maximum


type Strategy = Board -> Board -> Ordering


turn :: Strategy -> Board -> Maybe Board
turn s b =
  if length options > 0
    then Just $ maximumBy s options
    else Nothing
  where options = filter (/= b) $ map (flip pushTiles b) dirs

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
