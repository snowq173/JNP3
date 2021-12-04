{-# LANGUAGE OverloadedStrings #-}
import Data.String
import System.IO

type Program = IO ()
main :: Program
main = etap5

etap5 :: IO ()
etap5 = runActivity (withStartScreen (resettable (withUndo defaultActivity)))

data Event = KeyPress String
type Screen = String

type DrawFun = Integer -> Integer -> Char
type Picture = DrawFun -> DrawFun
blank = id
(&) = (.)

data Activity world = Activity
    world
    (Event -> world -> world)
    (world -> Screen)

data Coord = C Int Int
instance Eq Coord where
  C x y == C x' y' = (x == x') && (y == y')

data Tile = Wall | Ground | Storage | Box | Blank deriving Eq
data Maze = Maze Coord (Coord -> Tile)
data Direction = R | U | L | D deriving Eq

data State = S {  
  levels :: [Maze],
  levelNo :: Int,
  currentMaze :: Maze,
  playerCoord :: Coord,
  playerDirection :: Direction,
  movesNumber :: Int,
  reachableCoords :: [Coord],
  boxesCoords :: [Coord],
  storagesCoords :: [Coord]
}

instance Eq State where
   state1 == state2 =  (playerCoord state1 == playerCoord state2)
                    && (playerDirection state1 == playerDirection state2)
                    && (levelNo state1 == levelNo state2)

initial :: State
initial = S {
    levels = goodMazes,
    levelNo = 0,
    currentMaze = removeBoxes startingMaze,
    playerCoord = startCoord,
    playerDirection = U,
    movesNumber = 0,
    reachableCoords = reachableTiles,
    boxesCoords = filterArgsByFunctionValue mazeMap Box reachableTiles,
    storagesCoords = filterArgsByFunctionValue mazeMap Storage reachableTiles
  }
  where startingMaze = goodMazes!!0
        (Maze startCoord mazeMap) = startingMaze
        reachableTiles = reachableCoordsFromMaze startingMaze

defaultActivity :: Activity State
defaultActivity = Activity initial handleEvent draw

data WithUndo a = WithUndo a [a]
withUndo :: Eq a => Activity a -> Activity (WithUndo a)
withUndo (Activity state0 handle draw) = Activity state0' handle' draw' where
    state0' = WithUndo state0 []
    handle' (KeyPress key) (WithUndo s stack) | key == "u"
      = case stack of s':stack' -> WithUndo s' stack'
                      []        -> WithUndo s []
    handle' e              (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo (handle e s) (s:stack)
      where s' = handle e s
    draw' (WithUndo s _) = draw s

runActivity :: Activity s -> IO ()
runActivity (Activity state handle draw) = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  putStr "\ESCc"
  putStrLn (draw state)
  activityLoop (Activity state handle draw)

activityLoop :: Activity s -> IO ()
activityLoop (Activity state handleEvent drawState) = do
    input <- getChar
    let updatedState = handleEvent (KeyPress [input]) state
    putStr "\ESCc"
    putStrLn (drawState updatedState)
    activityLoop (Activity updatedState handleEvent drawState)

data SSState world = StartScreen | Running world

withStartScreen :: Activity s -> Activity (SSState s)
withStartScreen (Activity state0 handle draw)
  = Activity state0' handle' draw'
  where
    state0' = StartScreen
    
    handle' (KeyPress key) StartScreen
      | key == " "                     = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)
    
    draw'   StartScreen                = startScreen
    draw'   (Running s)                = draw s

resettable :: Activity s -> Activity s
resettable (Activity state0 handle draw)
  = Activity state0 handle' draw
  where handle' (KeyPress key) _ | (key == "\ESC") = state0
        handle' e s = handle e s

---------- Mazes lists ----------

goodMazes :: [Maze]
goodMazes = [maze1, maze2]

maze1 :: Maze
maze1 = Maze start mazeMap where
  start = C 3 (-3)
  mazeMap (C x y)
   | abs x > 4  || abs y > 4  = Blank
   | abs x == 4 || abs y == 4 = Wall
   | x ==  2    && y <= 0     = Wall
   | x ==  3    && y <= 0     = Storage
   | x >= -2    && y == 0     = Box
   | otherwise                = Ground
  
maze2 :: Maze
maze2 = Maze start mazeMap where
  start = C (-2) 2
  mazeMap (C x y)
   | abs x > 3  || abs y > 3  = Blank
   | abs x == 3 || abs y == 3 = Wall
   | x ==  -1   && abs y < 3  = Box
   | x ==  2    && abs y < 3  = Storage
   | otherwise                = Ground

badMazes :: [Maze]
badMazes = [maze3, maze4]

maze3 :: Maze
maze3 = Maze start mazeMap where
  start = C 0 0
  mazeMap (C x y)
   | abs x > 5  || abs y > 5  = Blank
   | abs x == 5 || abs y == 5 = Wall
   | abs x == 2 && abs y == 2 = Box
   | otherwise                = Ground
  
maze4 :: Maze
maze4 = Maze start mazeMap where
  start = C 1 1
  mazeMap (C x y)
   | abs x > 4  || abs y > 4  = Blank
   | x ==  2    && y <= 0     = Wall
   | x ==  3    && y <= 0     = Wall
   | x >= -2    && y == 0     = Box
   | otherwise                = Ground

removeBoxes :: Maze -> Maze
removeBoxes (Maze initial mazeMap) = (Maze initial refinedMazeMap)
  where refinedMazeMap coord
          | mazeMap coord == Box = Ground
          | otherwise = mazeMap coord

filterArgsByFunctionValue :: Eq b => (a -> b) -> b -> [a] -> [a]
filterArgsByFunctionValue fun val args = filterList predicate args
  where predicate = \x -> fun x == val

startScreen :: Screen
startScreen = "Sokoban ! Press SPACE to start the game"

---------- List functions ----------

elemList :: Eq a => a -> [a] -> Bool
elemList target lst = foldList (\el acc-> (acc || (el == target))) False lst

appendList :: [a] -> [a] -> [a]
appendList dstList srcList = foldList (\el acc -> el:acc) srcList dstList

listLength :: [a] -> Integer
listLength lst = foldList (\el acc -> acc + 1) 0 lst

filterList :: (a -> Bool) -> [a] -> [a]
filterList predicate lst = foldList (\el acc -> if (predicate el) then (acc ++ [el]) else acc) [] lst

mapList :: (a -> b) -> [a] -> [b]
mapList mapFun lst = foldList (\el acc -> (mapFun el):acc) [] lst

andList :: [Bool] -> Bool
andList lst = foldList (\b eval -> b && eval) True lst

allList :: (a-> Bool) -> [a] -> Bool
allList predicate lst = foldList (\el eval -> (predicate el) && eval) True lst

foldList :: (a -> b -> b) -> b -> [a] -> b
foldList fun acc (x:xs) = fun x (foldList fun acc xs)
foldList fun acc [] = acc

---------- Tile pictures ----------

boxTile :: Picture
boxTile drawFun = drawFun'
  where drawFun' 0 0 = '$'
        drawFun' x y = drawFun x y

groundTile :: Picture
groundTile drawFun = drawFun'
  where drawFun' 0 0 = ' '
        drawFun' x y = drawFun x y

storageTile :: Picture
storageTile drawFun = drawFun'
  where drawFun' 0 0 = '.'
        drawFun' x y = drawFun x y

wallTile :: Picture
wallTile drawFun = drawFun'
  where drawFun' 0 0 = '#'
        drawFun' x y = drawFun x y

translated :: Integer -> Integer -> Picture -> Picture
translated x y picture = (\drawFun -> (\x' y' -> picture (\x'' y'' -> drawFun (x'' + x) (y'' - y)) (x' - x) (y' + y)))

allNeighbours :: Coord -> [Coord]
allNeighbours (C x y) = [C (x-1) y, C x (y+1), C (x+1) y, C x (y-1)]

coordNeighbours :: (Coord -> Tile) -> Coord -> [Coord]
coordNeighbours mazeMap (C x y) = case mazeMap (C x y) of {
    Blank -> []; --once blank then not propagate further search
    otherwise -> filterList coordPredicate coordsToFilter;
  }
  where coordPredicate = not . \c -> (elemList (mazeMap c) [Wall])
        coordsToFilter = allNeighbours (C x y)

graphSearch :: Eq a => (a -> [a]) -> [a] -> [a] -> [a]
graphSearch neighboursFun visited [] = visited
graphSearch neighboursFun visited (v:vs)
  | elemList v visited = graphSearch neighboursFun visited vs
  | otherwise = graphSearch neighboursFun (visited ++ [v]) ((neighboursFun v) ++ vs)

listOfReachable :: Eq a => a -> (a -> [a]) -> [a] 
listOfReachable initial neighboursFun = graphSearch neighboursFun [] [initial]

isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighboursFun predicate = allList predicate (listOfReachable initial neighboursFun)

reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighboursFun = elemList v (listOfReachable initial neighboursFun)

allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighboursFun = allList (\v -> elemList v reachableVertices) vs
  where reachableVertices = listOfReachable initial neighboursFun

reachableCoordsFromMaze :: Maze -> [Coord]
reachableCoordsFromMaze (Maze initial mazeMap) = listOfReachable initial (coordNeighbours mazeMap)

isClosed :: Maze -> Bool
isClosed (Maze initial mazeMap) = (mazeMap initial == Ground || mazeMap initial == Storage)
                                && isGraphClosed initial neighboursFun closenessPredicate
  where neighboursFun = coordNeighbours mazeMap
        closenessPredicate = not . \c -> mazeMap c == Blank

isSane :: Maze -> Bool
isSane (Maze initial mazeMap) = reachableStoragesCount >= reachableBoxesCount
  where reachableCoords = reachableCoordsFromMaze (Maze initial mazeMap)
        reachableStoragesCount = listLength (filterList (\c -> mazeMap c == Storage) reachableCoords)
        reachableBoxesCount = listLength (filterList (\c -> mazeMap c == Box) reachableCoords)

playerPicture :: Picture
playerPicture drawFun = drawFun'
  where drawFun' 0 0 = '@'
        drawFun' x y = drawFun x y

drawPlayer :: State -> Picture
drawPlayer state = atCoord (playerCoord state) playerPicture

isOnLastLevel :: State -> Bool
isOnLastLevel state = (levelNo state) + 1 == length (levels state)

tilePicture :: Tile -> Picture
tilePicture ofType = case ofType of {
  Wall -> wallTile;
  Ground -> groundTile;
  Storage -> storageTile;
  Box -> boxTile;
  Blank -> blank;
}

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

mazeTileAtCoordNoBox :: (Coord -> Tile) -> Coord -> Picture
mazeTileAtCoordNoBox mazeMap coord
    | mazeTileAtCoord == Box = atCoord coord (tilePicture Ground)
    | otherwise = atCoord coord (tilePicture mazeTileAtCoord)
  where mazeTileAtCoord = mazeMap coord

decorateReachableCoords :: State -> [Coord]
decorateReachableCoords state = foldList (appendList) [] (mapList (\c -> allNeighbours c) (reachableCoords state))

currentMazePicture :: State -> Picture
currentMazePicture state = boxes & mazeNoBoxes
  where (Maze initial mazeMap) = currentMaze state
        decoratedCoords = decorateReachableCoords state
        mazeNoBoxes = foldList (&) blank (mapList (\c -> mazeTileAtCoordNoBox mazeMap c) decoratedCoords)
        boxes = foldList (&) blank (mapList (\c -> atCoord c (tilePicture Box)) (boxesCoords state))

draw :: State -> Screen
draw state
    | isWinning state && isOnLastLevel state = strLevelWon ++ " Press ESC to start new game."
    | isWinning state = strLevelWon
    | otherwise = [if x == 21 then '\n' else (screenBoard x y) | y<-[-12..12], x<-[-19..21]]
  where statePicture = drawPlayer state & currentMazePicture state
        screenBoard = statePicture (\x y -> ' ')
        strLevelWon = "Level finished. Total number of moves: " ++ (show (movesNumber state)) ++ "."

isWinning :: State -> Bool
isWinning state = andList (mapList predicate boxes)
  where boxes = boxesCoords state
        targetCoords = storagesCoords state
        predicate = \x -> elemList x targetCoords

adjacentCoord :: Direction -> Coord -> Coord 
adjacentCoord direction (C x y)
  | direction == R = (C (x+1) y)
  | direction == U = (C x (y+1))
  | direction == L = (C (x-1) y)
  | direction == D = (C x (y-1))

getCoordByEvent :: Event -> Coord -> Coord
getCoordByEvent (KeyPress key) c
  | key == "d" = adjacentCoord R c
  | key == "w" = adjacentCoord U c
  | key == "a" = adjacentCoord L c
  | key == "s" = adjacentCoord D c
getCoordByEvent _ c = c

isCoordAvailable :: Coord -> (Coord -> Tile) -> Direction -> Bool
isCoordAvailable coord mazeMap dir = case mazeMap coord of {
    Ground -> True;
    Box -> (tileToCheckIfBox == Ground) || (tileToCheckIfBox == Storage);
    Storage -> True;
    otherwise -> False;
  }
  where tileToCheckIfBox = mazeMap (adjacentCoord dir coord)

addBoxesToCurrentMaze :: State -> Maze
addBoxesToCurrentMaze state = (Maze initial refinedMaze)
  where (Maze initial mazeMap) = currentMaze state
        refinedMaze coord
          | elemList coord (boxesCoords state) = Box
          | otherwise = mazeMap coord

getUpdatedPlayerCoord :: Event -> State -> Coord
getUpdatedPlayerCoord event state
  | canMove = coordToCheck
  | otherwise = playerCoord state
  where (Maze initial mazeMap) = addBoxesToCurrentMaze state
        playerDir = getPlayerDirectionByEvent event state
        coordToCheck = getCoordByEvent event (playerCoord state)
        canMove = isCoordAvailable coordToCheck mazeMap playerDir

getPlayerDirectionByEvent :: Event -> State -> Direction
getPlayerDirectionByEvent (KeyPress key) state
  | key == "d" = R
  | key == "w" = U
  | key == "a" = L
  | key == "s" = D
getPlayerDirectionByEvent _ state = playerDirection state

goToNextLevel :: State -> State
goToNextLevel state = state {
    levelNo = (levelNo state) + 1,
    currentMaze = removeBoxes (Maze initial mazeMap),
    playerCoord = initial,
    playerDirection = U,
    movesNumber = 0,
    reachableCoords = reachableTiles,
    boxesCoords = filterArgsByFunctionValue mazeMap Box reachableTiles,
    storagesCoords = filterArgsByFunctionValue mazeMap Storage reachableTiles
  }
  where (Maze initial mazeMap) = (levels state)!!(levelNo state + 1)
        reachableTiles = reachableCoordsFromMaze (Maze initial mazeMap)

goToNextLevelOrStay :: State -> State
goToNextLevelOrStay state
  | not (isWinning state) = state
  | isOnLastLevel state = state
  | otherwise = goToNextLevel state

compareAndExchange :: Eq a => a -> a -> a -> a
compareAndExchange newValue desiredValue actualValue
  | actualValue == desiredValue = newValue
  | otherwise = actualValue

updateState :: Event -> State -> State
updateState (KeyPress key) state = case isWinning state of {
      True -> state;
      False -> state {
        playerCoord = updatedPlayerCoord,
        playerDirection = updatedPlayerDirection,
        boxesCoords = updatedBoxesCoords,
        movesNumber = (movesNumber state) +1
      };
    }
    where 
      updatedPlayerCoord = getUpdatedPlayerCoord (KeyPress key) state
      updatedPlayerDirection = getPlayerDirectionByEvent (KeyPress key) state
      movedBoxCoord = adjacentCoord updatedPlayerDirection updatedPlayerCoord
      updatedBoxesCoords = mapList (compareAndExchange movedBoxCoord updatedPlayerCoord) (boxesCoords state)

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) state
  | key == "n" = goToNextLevelOrStay state
  | otherwise = updateState (KeyPress key) state
