module Snake where

import Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)
import Control.Concurrent (MVar, ThreadId, forkIO, killThread, newEmptyMVar, putMVar, tryTakeMVar)
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.IO (BufferMode( NoBuffering ), stdin, stdout, hSetEcho, hSetBuffering)
import System.Random (RandomGen, StdGen, randomR, mkStdGen)

data StepDirection  =  DirUp
                    | DirDown
                    | DirLeft
                    | DirRight deriving (Eq, Show)   

data PathDirection = DirFromHead | DirFromTail deriving (Eq, Show)

type Point = (Int, Int)

type Snake = [Point]

type Walls = (Point, Point)

type Path = [Point]

type ClosedPath = [Point]

data WorldState = Process 
                | GameOver
                | Quit 
                | Hamilton deriving (Eq)   

data World = World  { snake :: Snake
                    , oldLast :: Point
                    , direction :: StepDirection
                    , fruit :: Point
                    , lastRenderTime :: UTCTime
                    , renderDelay :: NominalDiffTime
                    , isRenderIteration :: Bool
                    , worldState :: WorldState
                    , rand :: StdGen
                    , hamPath :: ClosedPath
                    }        

-- inits

initWorld :: UTCTime -> World
initWorld timePoint = World   { snake = [(4, y) | y <- [5..7]]
                              , oldLast = (0, 0)
                              , direction = DirRight
                              , fruit = (5, 5)
                              , lastRenderTime = timePoint
                              , renderDelay = 0.3
                              , isRenderIteration = True
                              , worldState = Process
                              , rand = mkStdGen 0
                              , hamPath = getHamPath wallsFirstPoint []
                              }                          

initWalls :: Walls
initWalls = ((1,1),(20,20))

wallsFirstPoint :: Point
wallsFirstPoint = ((fst $ fst initWalls) + 1, (snd $ fst initWalls) + 1)

clockwise = [DirUp, DirRight, DirDown, DirLeft]

-- snake move & collisions

snakeStep :: StepDirection -> Snake ->  Snake
snakeStep direction snake = (pointStep direction $ head snake):(init snake)

pointStep :: StepDirection -> Point -> Point
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
  
inputController :: Maybe Char -> World -> World
inputController command world = let
  boost dir1 dir2 = if dir1 == dir2 then 0.05 else 0.3
  filterSecondSegmentDir (x:[]) dirOld dirNew = dirNew
  filterSecondSegmentDir (x:xs) dirOld dirNew | pointStep dirNew x == head xs = dirOld
                                              | otherwise = dirNew in 
    case command of
    Just 'w' -> world { direction = filterSecondSegmentDir (snake world) (direction world) DirUp 
                      , renderDelay = boost (direction world) DirUp
                      , worldState = Process
                      }
    Just 's' -> world { direction = filterSecondSegmentDir (snake world) (direction world) DirDown
                      , renderDelay = boost (direction world) DirDown
                      , worldState = Process
                      }
    Just 'a' -> world { direction = filterSecondSegmentDir (snake world) (direction world) DirLeft 
                      , renderDelay = boost (direction world) DirLeft
                      , worldState = Process
                      }
    Just 'd' -> world { direction = filterSecondSegmentDir (snake world) (direction world) DirRight 
                      , renderDelay = boost (direction world) DirRight
                      , worldState = Process
                      }
    Just 'q' -> world { worldState = Quit }
    Just 'h' -> world { worldState = Hamilton
                      }
    _        -> world { renderDelay = if worldState world == Process then 0.3 else 0.05
                      }

timerController :: UTCTime -> World -> World
timerController timePoint world
  | isRenderTime timePoint world                      = world { lastRenderTime = timePoint
                                                              , isRenderIteration = True 
                                                              , oldLast = last $ snake world
                                                              }
  | otherwise                                         = world where
    isRenderTime timePoint world = diffUTCTime timePoint (lastRenderTime world) >= renderDelay world

moveController :: World -> World
moveController world 
  | not $ isRenderIteration world                     = world
  | worldState world == Process                       = world { snake = snakeStep (direction world) (snake world) }
  | otherwise                                         = world { snake = snakeStep botStepDir (snake world)
                                                              , hamPath = if botPathDir == DirFromTail then hamPath world else reverse $ hamPath world  
                                                              } where 
                                                                  (botStepDir, botPathDir) = nextBotDir (snake world) (fruit world) (hamPath world) 

collisionController :: World -> World
collisionController world
  | not $ isRenderIteration world = world
  | collisionSnake $ snake world                      = world { worldState = GameOver } 
  | collisionWall (head $ snake world) initWalls      = world { worldState = GameOver } 
  | collisionFruit (snake world) (fruit world)        = world { snake = (snake world) ++ [oldLast world]
                                                              , fruit = newFruit
                                                              , rand = newRand
                                                              }
  | otherwise                                         = world where
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
    updateStatus world
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

updateStatus :: World -> IO ()
updateStatus world                      | not $ isRenderIteration world = return ()
                                      | otherwise = do 
                                          menuStr ("snake length: " ++ (show (length $ snake world))) 1 initWalls
                                          if worldState world == Hamilton then do 
                                                menuStr ("game mode:  bot control") 2 initWalls else do 
                                                menuStr ("game mode: user control") 2 initWalls
                                              

-- loops
                                              
gameLoop :: ThreadId -> MVar Char -> World -> IO ()
gameLoop hInput input oldWorld = do
  tryInput <- tryTakeMVar input
  timePoint <- getCurrentTime
  let newWorld = collisionController . moveController $ timerController timePoint (inputController tryInput oldWorld) 
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

nextBotDir :: Snake -> Point -> ClosedPath -> (StepDirection, PathDirection)
nextBotDir snake fruit path | distBypass1 < distBypass2 && distBypass1 < distToFruit 
                              && not (collisionSnakeOnPath snake virtualBypassPoint path DirFromTail) 
                                = (dirBetweenPoints (head snake) virtualBypassPoint, DirFromTail)
                            | distBypass2 < distToFruit 
                              && not (collisionSnakeOnPath snake virtualBypassPoint path DirFromHead) 
                                = (dirBetweenPoints (head snake) virtualBypassPoint, DirFromHead) 
                            | otherwise = nextDirOnPath snake path where
  bypassDir = dirBetweenPoints (head snake) fruit
  virtualBypassPoint = pointStep bypassDir (head snake)
  (distBypass1, distBypass2) = distBetweenPointsOnPath virtualBypassPoint fruit path
  (distToFruit, _) = distBetweenPointsOnPath (head snake) fruit path

collisionSnakeOnPath :: Snake -> Point -> ClosedPath -> PathDirection -> Bool
collisionSnakeOnPath snake point path pathDir | null $ common snake pathPart = False
                                              | otherwise = True where
  _path = if pathDir == DirFromHead then path else reverse path
  pathPart = takePathPart point _path (length snake)
  common xs ys = [ x | x <- xs , y <- ys, x == y]
  takePathPart point path len = _takePathPart point (path ++ (take len path)) len where
    _takePathPart _     []     _    = []
    _takePathPart point (x:xs) len  | x == point = x:(take (len - 1) xs)
                                    | otherwise = _takePathPart point xs len

distBetweenPointsOnPath :: Point -> Point -> ClosedPath -> (Int, Int)
distBetweenPointsOnPath point1 point2 path  | id1 < id2 = (length path - id2 + id1,id2 - id1)
                                            | otherwise = (id1 - id2, length path - id1 + id2) where
  (id1,id2) = pointIndexOnPath (point1,point2) path 0 (0,0)
  pointIndexOnPath _               []     _   ids                     = ids
  pointIndexOnPath (point1,point2) (x:xs) acc (id1,id2) | x == point1 = pointIndexOnPath (point1,point2) xs (acc+1) (acc,id2)
                                                        | x == point2 = pointIndexOnPath (point1,point2) xs (acc+1) (id1,acc) 
                                                        | otherwise   = pointIndexOnPath (point1,point2) xs (acc+1) (id1,id2)   

dirBetweenPoints :: Point -> Point -> StepDirection
dirBetweenPoints (x1, y1) (x2, y2)  | x1 == x2 = if y1 > y2 then DirUp else DirDown
                                    | y1 == y2 = if x1 > x2 then DirLeft else DirRight 
                                    | otherwise = if abs (x1 - x2) < abs (y1 - y2) then 
                                        dirBetweenPoints (x1, 0) (x2, 0) else
                                        dirBetweenPoints (0, y1) (0, y2)  

nextDirOnPath :: Snake -> ClosedPath -> (StepDirection, PathDirection)
nextDirOnPath (snakeHead:snakeTail)  path   | snakeTail == [] = (dirBetweenPoints snakeHead (fst $ pointNeighborsOnPath snakeHead path), DirFromTail) 
                                            | point1 == head snakeTail = (dirBetweenPoints snakeHead point2, DirFromHead)
                                            | otherwise = (dirBetweenPoints snakeHead point1, DirFromTail) where 
                                                (point1, point2) = pointNeighborsOnPath snakeHead path

pointNeighborsOnPath :: Point -> ClosedPath -> (Point, Point)
pointNeighborsOnPath point path | not $ isPathContain path point || length path < 4 = error "incorrect initWalls"
                                | point == head path = (last path, head $ tail path)
                                | point == last path = (last $ init path, head path)
                                | otherwise = _pointNeighborsOnPath point path where
                                  _pointNeighborsOnPath point (a:b:c:xs) = if point == b then (a,c) else _pointNeighborsOnPath point (b:c:xs)
                                                         
getHamPath :: Point -> ClosedPath -> ClosedPath
getHamPath currentPoint hamPath  | hamPathCapacity initWalls == length (currentPoint:hamPath) 
                                   && distBetweenPoints currentPoint (last hamPath) == 1
                                      = currentPoint:hamPath
                                 | otherwise = getHamPath newPoint (currentPoint:hamPath) where
                                    newPoint = nextHamPathPoint (currentPoint:hamPath) clockwise
                                    hamPathCapacity ((x1, y1),(x2, y2)) = (x2 - x1 - 1) * (y2 - y1 - 1) 
                                    distBetweenPoints (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

nextHamPathPoint :: Path -> [StepDirection] -> Point
nextHamPathPoint _       []         = error "incorrect initWalls"
nextHamPathPoint hamPath (dir:dirs) | isPathContain hamPath virtualPoint || collisionWall virtualPoint initWalls = nextHamPathPoint hamPath dirs 
                                    | otherwise = virtualPoint where
  virtualPoint = pointStep dir (head hamPath)

-- main

main = do
  hSetBuffering stdin NoBuffering 
  hSetBuffering stdout NoBuffering 
  hSetEcho stdin False
  clearScreen
  input <- newEmptyMVar
  hInput <- forkIO $ inputLoop input
  timePoint <- getCurrentTime
  drawWalls '#' initWalls
  gameLoop hInput input (initWorld timePoint)