module Main where

import Prelude
import Data.String as String
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (concat, range)
import Data.Generic (class Generic, gShow)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))

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

data World = World
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
           ]
  where makeCell x y cell = Tuple (Point x y) cell

showRow :: World -> Int -> Int -> Int -> String
showRow (World world) minX maxX y =
  String.joinWith "" (showCell <$> range minX maxX)
  where
    showCell :: Int -> String
    showCell x =
      if (Point x y) == world.start
      then "S"
      else if (Point x y) == world.end
      then "E"
      else Map.lookup (Point x y) world.grid
           # map (const "#")
           # fromMaybe " "

showWorld :: World -> Point -> Point -> String
showWorld world (Point minX minY) (Point maxX maxY) =
  String.joinWith "\n" (showRow world minX maxX <$> range minY maxY)

------------------------------------------------------------

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
  let world = World { grid: initialGrid
                    , start: Point 1 5
                    , end: Point 18 15
                    }
  log (showWorld world (Point 0 0) (Point 20 20))
