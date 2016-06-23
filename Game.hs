import System.Random (randomRIO)
import Data.Matrix
import Data.Maybe (catMaybes)

newtype Tile = Tile Int deriving (Eq, Show)

defaultTile = Tile 1
tileChances = [(10, Tile 2)]

fromChances :: (Ord a, Num a) => b -> [(a, b)] -> a -> b
fromChances dflt [] _ = dflt
fromChances dflt ((chance, val):rest) number = if number < chance
  then val
  else fromChances dflt rest $ number - chance

tileFromInt = fromChances defaultTile tileChances

gridWidth = 4
gridHeight = 4

doubleTile :: Tile -> Tile
doubleTile (Tile i) = Tile $ i + 1

tileVisual :: Maybe Tile -> Int
tileVisual = (`orElse` 0) . fmap tileValue

tileValue :: Tile -> Int
tileValue (Tile i) = 2 ^ i

mergeList :: Eq a => (a -> a) -> [a] -> ([a], [a])
mergeList merge list = mergeListInternal merge (list, [])
  where
    mergeListInternal merge ([], merges) = ([], merges)
    mergeListInternal merge ([x], merges) = ([x], merges)
    mergeListInternal merge ((x:y:rest), merges) = if x == y
      then
        let
          (mergedRest, mergesRest) =
            mergeListInternal merge (rest, (merge x):merges)
        in
          ((merge x):mergedRest, mergesRest)
      else
        let
          (mergedRest, mergesRest) =
            mergeListInternal merge (y:rest, merges)
        in
          (x:mergedRest, mergesRest)

mergeTiles = mergeList doubleTile

applyTuple :: (a -> b, c -> d) -> (a, c) -> (b, d)
applyTuple (f0, f1) = \(a0, a1) -> (f0 a0, f1 a1)

-- Merges a grid to the left
moveGrid :: Matrix (Maybe Tile) -> (Matrix (Maybe Tile), [Tile])
moveGrid = applyTuple (fromLists, foldr1 (++)) . unzip . map moveRow . toLists

pad elem n list = if length list < n
  then elem:pad elem (n - 1) list
  else list
padR elem n = reverse . pad elem n . reverse

moveRow :: [Maybe Tile] -> ([Maybe Tile], [Tile])
moveRow list =
  let len = length list
      (merged, merges) = mergeTiles . catMaybes $ list
  in
    (padR Nothing len $ map Just merged, merges)

flipLR = fromLists . map reverse . toLists

data Movement = MoveUp | MoveDown | MoveLeft | MoveRight

getMovement :: IO Movement
getMovement = do
  putStrLn "Direction? h/j/k/l"
  chars <- getLine
  case (chars, toMovement $ head chars) of
    (_:_, Just c) -> return c
    _      -> getMovement

toMovement :: Char -> Maybe Movement
toMovement chr = case chr of
  'k' -> Just MoveUp
  'j' -> Just MoveDown
  'h' -> Just MoveLeft
  'l' -> Just MoveRight
  _   -> Nothing

printTiles = print . fmap tileVisual

getGridTransform :: Movement -> [Matrix a -> Matrix a]
getGridTransform input = case input of
  MoveUp -> [transpose]
  MoveDown -> [flipLR, transpose]
  MoveLeft -> [id]
  MoveRight -> [flipLR]

applyForwards :: [a -> a] -> (a -> a)
applyForwards = foldr1 (.)

applyBackwards :: [a -> a] -> (a -> a)
applyBackwards = foldr1 (.) . reverse

enumerate = zip [0..]
flattenList = foldr1 (++)
always x _ = x

getEmptyCoords :: Matrix (Maybe a) -> [(Int, Int)]
getEmptyCoords =
  catMaybes .
    map coordsIfNothing .
    flattenList .
    map taggedListToListOfTags .
    enumerate .
    map enumerate .
    toLists
  where
    coordsIfNothing (y, (x, oEl)) = case oEl of
      Nothing -> Just (x, y)
      _       -> Nothing
    taggedListToListOfTags (tag, list) = map ((,) tag) list

gameIsWon :: Matrix (Maybe Tile) -> Bool
gameIsWon = any ((==) . Just $ Tile 11) . toList

gameLoop :: Int -> Int -> Matrix (Maybe Tile) -> IO ()
gameLoop bestScore score grid =
  if gameIsWon grid then do
    putStrLn ""
    putStrLn $ "You won! Your score is " ++ show score
  else do
    putStrLn $ "Score: " ++ show score ++ ", Best: " ++ show bestScore
    printTiles grid
    movement <- getMovement
    loopWithInput bestScore score grid movement

setCartesian elem (x, y) = setElem elem (y + 1, x + 1)

-- TODO: Use a monad transformer instead of IO
randomElem :: [a] -> IO (Maybe a)
randomElem [] = return Nothing
randomElem list@(_:_) = randomRIO (0, length list - 1) >>=
  return . Just . (!!) list

-- TODO: Use a monad transformer instead of IO
addTile :: Matrix (Maybe Tile) -> IO (Maybe (Matrix (Maybe Tile)))
addTile grid = do
  maybeElem <- (randomElem . getEmptyCoords $ grid)
  return $ fmap (\e -> setCartesian (Just defaultTile) e grid) maybeElem

maybe `orElse` dflt = case maybe of
  Nothing  -> dflt
  Just val -> val

loopWithInput :: Int -> Int -> Matrix (Maybe Tile) -> Movement -> IO ()
loopWithInput bestScore score grid input =
  let transform = getGridTransform input
      (movedGrid, merges) =
        applyTuple (applyBackwards transform, id) .
          moveGrid .
          applyForwards transform $
          grid
      newScore = score + (sum . map tileValue $ merges)
      newBest = max bestScore newScore
  in do
    newGrid <- addTile movedGrid
    fmap (gameLoop newBest newScore) newGrid `orElse`
      newGame newBest

newGame bestScore = do
  putStrLn ""
  putStrLn "New game"
  putStrLn ""
  gameLoop bestScore 0 initialGrid

minimalGrid = fromLists [[Just $ Tile 1]]

fromMinimal = extendTo Nothing gridWidth gridHeight

initialGrid = fromMinimal minimalGrid

main = newGame 0
