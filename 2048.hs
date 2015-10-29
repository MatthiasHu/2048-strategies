import Data.Char (intToDigit)
import Data.Maybe (catMaybes)
import Data.List (transpose, intercalate)


boardSize = 4


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

instance Show Board where
  show (Board rows) =
    concatMap (\row -> map digit row ++ "\n") rows

digit :: Maybe Int -> Char
digit Nothing = ' '
digit (Just n) = intToDigit n

emptyBoard :: Board
emptyBoard = Board $ replicate boardSize $ replicate boardSize Nothing


