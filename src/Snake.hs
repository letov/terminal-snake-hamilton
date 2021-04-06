module Snake where

import Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)
import Control.Concurrent (MVar, ThreadId, forkIO, killThread, newEmptyMVar, putMVar, tryTakeMVar)
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.IO (BufferMode( NoBuffering ), stdin, stdout, hSetEcho, hSetBuffering)
import System.Random (RandomGen, StdGen, randomR, mkStdGen)

data Direction =  DirUp
                | DirDown
                | DirLeft
                | DirRight deriving (Eq,Show)   

type Point = (Int, Int)

type Snake = [Point]

type Walls = (Point, Point)

type Path = [Point]

type ClosedPath = [Point]

data WorldState = Process 
                | GameOver
                | Quit 
                | Hamilton deriving (Eq,Show)   

data World = World  { snake :: Snake
                    , oldLast :: Point
                    , direction :: Direction
                    , fruit :: Point
                    , lastRenderTime :: UTCTime
                    , renderDelay :: NominalDiffTime
                    , isRenderIteration :: Bool
                    , worldState :: WorldState
                    , rand :: StdGen
                    , hamPath :: Maybe ClosedPath
                    }        

-- inits

initWorld :: UTCTime -> World
initWorld timePoint = World   { snake = [(5, 10)]
                              , oldLast = (0, 0)
                              , direction = DirRight
                              , fruit = (15, 12)
                              , lastRenderTime = timePoint
                              , renderDelay = 0.3
                              , isRenderIteration = True
                              , worldState = Process
                              , rand = mkStdGen 0
                              , hamPath = getHamPath (Just wallsFirstPoint) []
                              }                          

initWalls :: Walls
initWalls = ((1,1),(40,20))

wallsFirstPoint :: Point
wallsFirstPoint = ((fst $ fst initWalls) + 1, (snd $ fst initWalls) + 1)

clockwise = [DirUp, DirRight, DirDown, DirLeft]

-- snake move & collisions

snakeStep :: Direction -> Snake ->  Snake
snakeStep direction snake = (pointStep direction $ head snake):(init snake)

pointStep :: Direction -> Point -> Point
pointStep direction (x, y) = case direction of
  DirUp -> (x, y - 1) 
  DirDown -> (x, y + 1)
  DirLeft -> (x - 1, y)
  DirRight -> (x + 1, y)

collisionSnake :: Snake -> Bool
collisionSnake (x:xs) = isPathContain xs x

isPathContain :: Path -> Point -> Bool
isPathContain path point = any (== point) path

collisionWall :: Point -> Walls -> Bool
collisionWall (sx,sy) ((wx1,wy1),(wx2,wy2)) = sx <= wx1 || sx >= wx2 || sy <= wy1 || sy >= wy2  

-- world controllers
  
inputWorldController :: Maybe Char -> World -> World
inputWorldController command world = let
  boost dir1 dir2 = if dir1 == dir2 then 0.05 else 0.3
  filterSecondSegmentDir (x:[]) dirOld dirNew = dirNew
  filterSecondSegmentDir (x:xs) dirOld dirNew | pointStep dirNew x == head xs = dirOld
                                              | otherwise = dirNew in 
    case command of
    Just 'w' -> world { direction = filterSecondSegmentDir (snake world) (direction world) DirUp 
                      , renderDelay = boost (direction world) DirUp
                      }
    Just 's' -> world { direction = filterSecondSegmentDir (snake world) (direction world) DirDown
                      , renderDelay = boost (direction world) DirDown
                      }
    Just 'a' -> world { direction = filterSecondSegmentDir (snake world) (direction world) DirLeft 
                      , renderDelay = boost (direction world) DirLeft
                      }
    Just 'd' -> world { direction = filterSecondSegmentDir (snake world) (direction world) DirRight 
                      , renderDelay = boost (direction world) DirRight
                      }
    Just 'q' -> world { worldState = Quit }
    Just 'h' -> world { worldState = Hamilton
                      }
    _        -> world { renderDelay = 0.3
                      }

botWorldController :: World -> World
botWorldController world 
  | worldState world == Process = world
  | hamPath world == Nothing = world { worldState = Process }
  | otherwise = world { direction = direction
                      , renderDelay = 0.05 
                      , hamPath = if rev then reverseHamPath $ hamPath world else hamPath world 
                      } where 
                        (direction,rev) = nextBotDir (snake world) (fruit world) (hamPath world)
                        reverseHamPath (Just hamPathValue) = Just $ reverse hamPathValue 

timeWorldController :: UTCTime -> World -> World
timeWorldController timePoint world
  | isRenderTime timePoint world              = world { snake = snakeStep (direction world) (snake world)
                                                      , lastRenderTime = timePoint
                                                      , isRenderIteration = True 
                                                      , oldLast = last $ snake world
                                                      }
  | collisionSnake $ snake world               = world { worldState = GameOver } 
  | collisionWall (head $ snake world) initWalls     = world { worldState = GameOver } 
  | collisionFruit (snake world) (fruit world) = world { snake = (snake world) ++ [oldLast world]
                                                      , fruit = newFruit
                                                      , rand = newRand
                                                      }
  | otherwise                                 = world where
    isRenderTime timePoint world = diffUTCTime timePoint (lastRenderTime world) >= renderDelay world
    collisionFruit snake fruit = fruit == head snake
    (newFruit, newRand) = freeRandomPoint world (rand world)
    randomPoint ((minX, minY), (maxX, maxY)) g = let
      (x, g1) = randomR (minX + 1, maxX - 1) g
      (y, g2) = randomR (minY + 1, maxY - 1) g1 in 
        ((x, y), g2)
    freeRandomPoint world g | not $ elem point ((fruit world):(snake world)) = (point, g1)
                            | otherwise = freeRandomPoint world g1 where
                                (point, g1) = randomPoint initWalls g

-- graphics

renderWorld :: World ->  MVar Char -> IO ()
renderWorld world input
  | not $ isRenderIteration world = return ()
  | otherwise = do
    drawPoint '@' (fruit world)  
    drawPoint ' ' (oldLast world)
    mapM_ (drawPoint 'O') (snake world)
    updateMenu world initWalls
    setCursorPosition 0 0 

drawPoint :: Char -> Point -> IO ()
drawPoint char (x, y) = setCursorPosition y x >> putChar char   

menuStr :: String -> Int -> Walls -> IO ()
menuStr str i ((_, y1),(x2, _)) = do
  setCursorPosition (y1+i) (x2+2)
  putStr str

drawWalls :: Char -> Walls -> IO ()
drawWalls char ((x1, y1),(x2, y2)) = do
  mapM_ (drawPoint char) [(x1, y)| y <- [y1..y2]]
  mapM_ (drawPoint char) [(x, y1)| x <- [x1..x2]]
  mapM_ (drawPoint char) [(x2, y)| y <- [y1..y2]] 
  mapM_ (drawPoint char) [(x, y2)| x <- [x1..x2]]
  menuStr "w - Up" 4 initWalls
  menuStr "s - Down" 5 initWalls
  menuStr "a - Left" 6 initWalls
  menuStr "d - Right" 7 initWalls
  menuStr "hold - speed boost" 8 initWalls
  menuStr "h - Hamilton mode" 9 initWalls
  menuStr "q - Quit" 10 initWalls

updateMenu :: World -> Walls -> IO ()
updateMenu world ((x1, y1),(x2, y2))  | not $ isRenderIteration world = return ()
                                      | (length $ snake world) > 1 = do 
                                          showGameStatus
                                          menuStr emptyStr 9 initWalls     
                                      | worldState world == Hamilton = do
                                          showGameStatus
                                          clearMenu [4..10]
                                          menuStr "q - Quit" 4 initWalls
                                      | otherwise = do showGameStatus where
                                            clearMenu [] = return ()
                                            clearMenu (x:xs) = do 
                                              menuStr emptyStr x initWalls
                                              clearMenu xs
                                            emptyStr = take 18 $ repeat ' '
                                            showGameStatus = do 
                                              menuStr ("snake length: " ++ (show (length $ snake world))) 1 initWalls
                                              if worldState world == Hamilton then do 
                                                menuStr ("game mode:  bot control") 2 initWalls else do 
                                                menuStr ("game mode: user control") 2 initWalls

-- loops
                                              
gameLoop :: ThreadId -> MVar Char -> World -> IO ()
gameLoop hInput input oldWorld = do
  tryInput <- tryTakeMVar input
  timePoint <- getCurrentTime
  let newWorld = timeWorldController timePoint (botWorldController (inputWorldController tryInput oldWorld)) 
  renderWorld newWorld input
  case worldState newWorld of
    GameOver -> do
      clearScreen 
      drawWalls '#' initWalls
      gameLoop hInput input (initWorld timePoint)
    Quit     -> killThread hInput 
    _        -> gameLoop hInput input newWorld { isRenderIteration = False }  

inputLoop :: MVar Char -> IO ()
inputLoop input = (putMVar input =<< getChar) >> inputLoop input 

-- game bot

-- (следующее направление змеи, инверсия пути)
nextBotDir :: Snake -> Point -> Maybe Path -> (Direction, Bool)
nextBotDir _                     _     Nothing     = undefined
nextBotDir (snakeHead:snakeTail) fruit (Just path) | otherwise = undefined where
  bypassDir = dirBetweenPoints snakeHead fruit
  virtualBypassPoint = pointStep bypassDir snakeHead
  (distPath1, distPath2) = distBetweenPointsOnPath virtualBypassPoint fruit path

-- расстояние между двумя точками на поле
distBetweenPoints :: Point -> Point -> Int
distBetweenPoints (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- расстояние между двумя точками на пути (прямое, обратное)
distBetweenPointsOnPath :: Point -> Point -> ClosedPath -> (Int, Int)
distBetweenPointsOnPath point1 point2 path  | id1 < id2 = (length path - id2 + id1,id2 - id1)
                                            | otherwise = (id1 - id2, length path - id1 + id2) where
  (id1,id2) = pointIndexOnPath (point1,point2) path 0 (0,0)
  pointIndexOnPath _               []     _   ids                     = ids
  pointIndexOnPath (point1,point2) (x:xs) acc (id1,id2) | x == point1 = pointIndexOnPath (point1,point2) xs (acc+1) (acc,id2)
                                                        | x == point2 = pointIndexOnPath (point1,point2) xs (acc+1) (id1,acc) 
                                                        | otherwise   = pointIndexOnPath (point1,point2) xs (acc+1) (id1,id2)   

-- направление между двумя точками
dirBetweenPoints :: Point -> Point -> Direction
dirBetweenPoints (x1, y1) (x2, y2)  | x1 == x2 = if y1 > y2 then DirUp else DirDown
                                    | y1 == y2 = if x1 > x2 then DirLeft else DirRight 
                                    | otherwise = if abs (x1 - x2) < abs (y1 - y2) then 
                                        dirBetweenPoints (x1, 0) (x2, 0) else
                                        dirBetweenPoints (0, y1) (0, y2)  

-- (следующее направление для змеи по пути, инверсия пути) 
nextDirOnPath :: Snake -> ClosedPath -> (Direction, Bool)
nextDirOnPath (snakeHead:snakeTail)  path   | snakeTail == [] = (dirBetweenPoints snakeHead (fst $ pointNeighborsOnPath snakeHead path), False) 
                                            | point1 == head snakeTail = (dirBetweenPoints snakeHead point2, False)
                                            | otherwise = (dirBetweenPoints snakeHead point1, False) where 
                                                (point1, point2) = pointNeighborsOnPath snakeHead path

-- получить пару соседних точек на пути
pointNeighborsOnPath :: Point -> ClosedPath -> (Point, Point)
pointNeighborsOnPath point path | not $ isPathContain path point || length path < 4 = undefined
                                | point == head path = (last path, head $ tail path)
                                | point == last path = (last $ init path, head path)
                                | otherwise = _pointNeighborsOnPath point path where
                                  _pointNeighborsOnPath point (a:b:c:xs) = if point == b then (a,c) else _pointNeighborsOnPath point (b:c:xs)

-- получить цикл Гамильтона из (0,0) в рамках initWalls                                                          
getHamPath :: Maybe Point -> ClosedPath -> Maybe ClosedPath
getHamPath Nothing             _                        = Nothing
getHamPath (Just currentPoint) hamPath  | hamPathCapacity initWalls == length (currentPoint:hamPath) 
                                          && distBetweenPoints currentPoint (last hamPath) == 1
                                                        = Just (currentPoint:hamPath)
                                        | otherwise     = getHamPath newPoint (currentPoint:hamPath) where
                                          newPoint = nextHamPathPoint Nothing (currentPoint:hamPath) clockwise
                                          hamPathCapacity ((x1, y1),(x2, y2)) = (x2 - x1 - 1) * (y2 - y1 - 1) 

-- получить следующую точку цикла Гамильтона, перебрав все направления по clockwise
nextHamPathPoint :: Maybe Point -> Path -> [Direction] -> Maybe Point
nextHamPathPoint Nothing              _       []         = Nothing
nextHamPathPoint (Just newPointValue) _       _          = Just newPointValue
nextHamPathPoint _                    hamPath (dir:dirs) = nextHamPathPoint checkedVirtualPoint hamPath dirs where
  checkedVirtualPoint | isPathContain hamPath virtualPoint || collisionWall virtualPoint initWalls  = Nothing
                      | otherwise                                                                   = Just virtualPoint
  virtualPoint = pointStep dir (head hamPath)

-- main

main' = do
  hSetBuffering stdin NoBuffering 
  hSetBuffering stdout NoBuffering 
  hSetEcho stdin False
  clearScreen
  input <- newEmptyMVar
  hInput <- forkIO $ inputLoop input
  timePoint <- getCurrentTime
  drawWalls '#' initWalls
  gameLoop hInput input (initWorld timePoint)