module SnakeTest where

import Test.HUnit
import System.Exit
import Snake

hamPathReference = [(2,3),(2,4),(2,5),(3,5),(3,4),(3,3),(4,3),(4,4),(4,5),(5,5),(5,4),(5,3),(5,2),(4,2),(3,2),(2,2)]

main = do 
 counts <- runTestTT ( test [
  TestCase $ assertEqual "   !!!" (Just (5,9)) (nextHamPathPoint Nothing [(5, 10)] clockwise),
  TestCase $ assertEqual "   !!!" (Just (3,2)) (nextHamPathPoint Nothing [(3, 3)] clockwise),
  TestCase $ assertEqual "   !!!" ((2,2),(2,4)) (pointNeighborsOnPath (2,3) hamPathReference),
  TestCase $ assertEqual "   !!!" ((2,5),(3,4)) (pointNeighborsOnPath (3,5) hamPathReference),
  TestCase $ assertEqual "   !!!" ((3,2),(2,3)) (pointNeighborsOnPath (2,2) hamPathReference),
  TestCase $ assertEqual "   !!!" (DirUp, False) (nextDirOnPath [(2,3),(2,4)] ( hamPathReference)), 
  TestCase $ assertEqual "   !!!" (DirDown, False) (nextDirOnPath [(2,3),(2,2)] ( hamPathReference)), 
  TestCase $ assertEqual "   !!!" (DirRight, False) (nextDirOnPath [(2,2),(2,3)] ( hamPathReference)), 
  TestCase $ assertEqual "   !!!" (DirRight, False) (nextDirOnPath [(2,2)] ( hamPathReference)), 
  TestCase $ assertEqual "   !!!" (DirUp, False) (nextDirOnPath [(2,3)] ( hamPathReference)), 
  TestCase $ assertEqual "   !!!" (13, 3) (distBetweenPointsOnPath (4,3) (5,5) hamPathReference), 
  TestCase $ assertEqual "   !!!" (5, 11) (distBetweenPointsOnPath (2,5) (4,2) hamPathReference), 
  TestCase $ assertEqual "   !!!" (8, 8) (distBetweenPointsOnPath (5,3) (3,5) hamPathReference), 
  TestCase $ assertEqual "   !!!" (11, 5) (distBetweenPointsOnPath (5,2) (2,4) hamPathReference), 
  TestCase $ assertEqual "   !!!" (11, 5) (distBetweenPointsOnPath (5,2) (2,4) hamPathReference), 
  TestCase $ assertEqual "   !!!" (DirRight) (dirBetweenPoints (0,0) (2,4)), 
  TestCase $ assertEqual "   !!!" (DirDown) (dirBetweenPoints (0,0) (4,2)), 
  TestCase $ assertEqual "   !!!" 1 1
  ])
 if (errors counts + failures counts == 0)
  then exitSuccess
  else exitFailure