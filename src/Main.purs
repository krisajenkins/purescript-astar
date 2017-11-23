module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (concat, range, sortWith)
import Data.Array as Array
import Data.Generic (class Generic, gShow)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)

data Cell = Rock

derive instance genericCell :: Generic Cell
instance showCell :: Show Cell where
  show = gShow

data Point = Point Int Int

derive instance eqPoint :: Eq Point
derive instance ordPoint :: Ord Point
derive instance genericPoint :: Generic Point
instance showPoint :: Show Point where
  show = gShow

type Grid = Map Point Cell

type World =
  { grid :: Grid
  , start :: Point
  , end :: Point
  }

------------------------------------------------------------

initialGrid :: Grid
initialGrid =
  Map.fromFoldable $
    concat [ makeCell <$> [5] <*> range 3 10 <*> [Rock]
           , makeCell <$> [12] <*> range 5 15 <*> [Rock]
           , makeCell <$> [8] <*> range 2 3 <*> [Rock]
           ]
  where makeCell x y cell = Tuple (Point x y) cell

showRow :: World -> Path -> Int -> Int -> Int -> String
showRow world path minX maxX y =
  String.joinWith "" (showACell <$> range minX maxX)
  where
    showACell :: Int -> String
    showACell x =
      if (Point x y) == world.start
      then "S"
      else if (Point x y) == world.end
      then "E"
      else if Set.member (Point x y) path
      then "."
      else Map.lookup (Point x y) world.grid
           # map (const "▒")
           # fromMaybe " "

showWorld :: World -> Path -> Point -> Point -> String
showWorld world path (Point minX minY) (Point maxX maxY) =
  String.joinWith "\n" (showRow world path minX maxX <$> range minY maxY)

------------------------------------------------------------

type State =
  { openSet :: Set Point
  , closedSet :: Set Point
  , cameFrom :: Map Point Point
  , knownCost :: Map Point Int
  , current :: Point
  }

initialState :: Point -> State
initialState start =
  { openSet: Set.fromFoldable [start]
  , closedSet: Set.empty
  , cameFrom: Map.empty
  , knownCost: Map.fromFoldable [Tuple start 0]
  , current: start
  }

------------------------------------------------------------
type Path = Set Point

solve :: World -> Path
solve world =
  initialState world.start
    # solveStep world
    # reconstructPath
    # Set.fromFoldable

solveStep :: World -> State -> State
solveStep world state =
  if Set.isEmpty state.openSet
  then state
  else
    case selectNext of
         Nothing -> state
         Just current | current == world.end -> state { current = current }
         Just current ->
               let newState = state { openSet = Set.union (Set.fromFoldable (neighbours current))
                                                          (Set.delete current state.openSet)
                                    , closedSet = Set.insert current state.closedSet
                                    , current = current
                                    }
                   reducer :: State -> Point -> State
                   reducer s item =
                     case Map.lookup current s.knownCost of
                       Nothing -> s { openSet = Set.empty }
                       Just gScore -> case Map.lookup item s.knownCost of
                                        Just oldScore | oldScore <= gScore + 1 -> s
                                        _ ->  s { cameFrom = Map.insert item current s.cameFrom
                                                , knownCost = Map.insert item (gScore + 1) s.knownCost
                                                }
               in solveStep world $ Array.foldl reducer newState (neighbours current)
  where selectNext :: Maybe Point
        selectNext = state.openSet
                     # Array.fromFoldable
                     # sortWith score
                     # Array.head

        score :: Point -> Maybe Int
        score point = (+) <$> Map.lookup point state.knownCost <*> Just (heuristicScore point)

        heuristicScore :: Point -> Int
        heuristicScore (Point px py) =
           let (Point wx wy) = world.end
           in abs (px - wx) + abs (py - wy)

        neighbours :: Point -> Array Point
        neighbours (Point x y) =
            [ Point (x - 1) y
            , Point (x + 1) y
            , Point x (y - 1)
            , Point x (y + 1)
            ]
          # Array.filter (not (flip Set.member state.closedSet))
          # Array.filter (not (flip Map.member world.grid))

reconstructPath :: State -> Array Point
reconstructPath state =
  unfoldr foo state.current
  where foo :: Point -> Maybe (Tuple Point Point)
        foo now = (Tuple now) <$> Map.lookup now state.cameFrom

------------------------------------------------------------

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let world = { grid: initialGrid
              , start: Point 1 5
              , end: Point 18 11
              }
      path = solve world
  log (showWorld world path (Point 0 0) (Point 20 20))
