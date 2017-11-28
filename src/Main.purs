module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (concat, range, sortWith)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Generic (class Generic, gShow)
import Data.Lens (Lens', over)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Ord (abs)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Symbol (SProxy(..))
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

type Grid = Map Point Cell

type World =
  { grid :: Grid
  , start :: Point
  , end :: Point
  }

------------------------------------------------------------

showRow :: World -> Path -> Int -> Int -> Int -> String
showRow world path minX maxX y =
  String.joinWith "" (showAPoint <$> range minX maxX)
  where
    showAPoint :: Int -> String
    showAPoint x
      | (Point x y) == world.start = "S"
      | (Point x y) == world.end   = "E"
      | Set.member (Point x y) path = "."
      | Map.member (Point x y) world.grid = "▒"
      | otherwise = " "

showWorld :: World -> Path -> Point -> Point -> String
showWorld world path (Point minX minY) (Point maxX maxY) =
  String.joinWith "\n" (showRow world path minX maxX <$> range minY maxY)

------------------------------------------------------------

type State =
  { openSet :: Set Point
  , closedSet :: Set Point
  , cameFrom :: Map Point Point
  , knownCost :: Map Point Int
  }

_openSet :: Lens' State (Set Point)
_openSet = prop (SProxy :: SProxy "openSet")

_closedSet :: Lens' State (Set Point)
_closedSet = prop (SProxy :: SProxy "closedSet")

_cameFrom :: Lens' State (Map Point Point)
_cameFrom = prop (SProxy :: SProxy "cameFrom")

_knownCost :: Lens' State (Map Point Int)
_knownCost = prop (SProxy :: SProxy "knownCost")

initialState :: Point -> State
initialState start =
  { openSet: Set.fromFoldable [start]
  , closedSet: Set.empty
  , cameFrom: Map.empty
  , knownCost: Map.fromFoldable [Tuple start 0]
  }

------------------------------------------------------------
type Path = Set Point

solve :: World -> Path
solve world =
  initialState world.start
    # solveStep world
    # reconstructPath world.end
    # Set.fromFoldable

solveStep :: World -> State -> State
solveStep world state =
  case selectNext state.openSet of
    Nothing -> state
    Just current | current == world.end -> state
    Just current ->
      let next = validNeighbours current

          newState = state
                        # over _openSet   (Set.delete current)
                        # over _closedSet (Set.insert current)
                        # over _openSet   (Set.union next)

          reducer :: State -> Point -> State
          reducer s item =
            let currentScore = fromMaybe 0 (Map.lookup current s.knownCost)
                thisScore = currentScore + 1
            in case Map.lookup item s.knownCost of
                 Just oldScore | oldScore <= thisScore -> s
                 _ -> s
                        # over _cameFrom  (Map.insert item current)
                        # over _knownCost (Map.insert item thisScore)
      in solveStep world $ foldl reducer newState next
  where
    selectNext :: Set Point -> Maybe Point
    selectNext =
      Array.fromFoldable
        >>> sortWith score
        >>> Array.head

    score :: Point -> Maybe Int
    score point = do
      knownCost <- Map.lookup point state.knownCost
      heuristicCost <- Just $ heuristicScore point world.end
      pure $ knownCost + heuristicCost

    validNeighbours :: Point -> Set Point
    validNeighbours =
        neighbours
          >>> Array.filter (do inClosed <- flip Set.member state.closedSet
                               inGrid   <- flip Map.member world.grid
                               pure $ not (inClosed || inGrid))
          >>> Set.fromFoldable

neighbours :: Point -> Array Point
neighbours (Point x y) =
  [ Point (x - 1) y
  , Point (x + 1) y
  , Point x (y - 1)
  , Point x (y + 1)
  ]

heuristicScore :: Point -> Point -> Int
heuristicScore (Point ax ay) (Point bx by) =
  abs (bx - ax) + abs (by - ay)

reconstructPath :: Point -> State -> Array Point
reconstructPath end state =
  unfoldr step end
  where
    step :: Point -> Maybe (Tuple Point Point)
    step now = Tuple now <$> Map.lookup now state.cameFrom

------------------------------------------------------------

initialGrid :: Grid
initialGrid =
  Map.fromFoldable $
    concat [ makeCell <$> [5]  <*> range 3 10 <*> [Rock]
           , makeCell <$> [12] <*> range 5 15 <*> [Rock]
           , makeCell <$> [8]  <*> range 2 3  <*> [Rock]
           ]
  where makeCell x y cell = Tuple (Point x y) cell

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let world = { grid: initialGrid
              , start: Point 1 5
              , end: Point 18 11
              }
      path = solve world
  log (showWorld world path (Point 0 0) (Point 20 20))
