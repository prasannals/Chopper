module Main where

import Data.Array
import Data.String
import Halogen.VDom.Types
import Prelude
import Types
import UI.Elements
import UI.Events
import UI.Properties

import Neon.Operator ((%))
import Data.Array (length)
import Control.Monad.Eff.Random (randomInt)
import Control.Monad.Eff.Exception (stack)
import Control.Plus ((<|>))
import DOM.HTML.Event.ErrorEvent (lineNo)
import DOM.HTML.History (back)
import Data.Array (group)
import FRP as F
import FRP.Behavior (behavior)
import FRP.Event as E
import FRP.Event.Time (animationFrame)
import Halogen.VDom.Types (graft)
import UI.Core (MEvent, AttrValue(..), Attr(..), Prop)
import UI.Util as U
import Collision

foreign import click :: MEvent
foreign import change :: MEvent


heliInitX :: Int
heliInitX = 50

heliInitY :: Int
heliInitY = 50


clickIncY :: Int
clickIncY = -50

clickIncX :: Int
clickIncX = 0

numObstaclesOnScreen :: Int
numObstaclesOnScreen = 4

fallIncX :: Int
fallIncX = 0

fallIncY :: Int
fallIncY = 3

gameAreaHeight :: Int
gameAreaHeight = 800

gameAreaWidth :: Int
gameAreaWidth = 1200

type Pos2d = {x :: Int , y :: Int}
type Obstacle = {pos :: Pos2d, mWidth :: Int, mHeight :: Int, id :: Int, background :: String}
type MyState = {background :: String, heliPos :: Pos2d, obstacles :: Array Obstacle, numObstacles :: Int}



widget :: forall a . MyState  -> VDom Attr a
widget state = linearLayout
              [ id_ "root"
              , height "match_parent"
              , width "match_parent"
              , background "#76b852"
              , gravity "center"
              , orientation "vertical"
              ]
              [
                frameLayout
                [
                  id_ "gameArea"
                  , height (show gameAreaHeight)
                  , width (show gameAreaWidth)
                  , background (state.background)
                  , onClick (Some click)
                ]
                (([
                  imageView[
                   id_ "heli"
                   , height "100"
                   , width "150"
                   , imageUrl "heli"
                   , margin (posToMarginStr state.heliPos.x state.heliPos.y)
                  ]
                ]) <> (drawObstacle <$> state.obstacles)  )

              ]

posToMarginStr :: Int -> Int -> String
posToMarginStr x y = (show x) <> ", " <> (show y) <> " , 0, 0"


drawObstacle :: forall a. Obstacle -> VDom Attr a
drawObstacle obstacle = frameLayout [
    id_ (show $ obstacle.id),
    height (show $ obstacle.mHeight),
    width (show $ obstacle.mWidth),
    background obstacle.background,
    margin (posToMarginStr obstacle.pos.x obstacle.pos.y)
  ][]

getObstacles :: Array Obstacle
getObstacles = [{pos : {x : 100, y : 0 }, mWidth : 50, mHeight : 400, id : 0, background : "#000000"},
                {pos : {x : 250, y : 0 }, mWidth : 50, mHeight : 400, id : 1, background : "#000000"},
                {pos : {x : 550, y : 0 }, mWidth : 50, mHeight : 400, id : 2, background : "#000000"},
                {pos : {x : 750, y : 0 }, mWidth : 50, mHeight : 400, id : 3, background : "#ff0000"}]


updateObstaclePos :: Int -> Int -> Obstacle -> Obstacle
updateObstaclePos dx dy {pos : {x, y}, mWidth, mHeight, id, background }= {pos : {x: (x + dx), y : (y + dy)}, mWidth, mHeight, id, background }


eval x = do
      (state::MyState) <- U.getState
      U.updateState "heliPos" {x : state.heliPos.x + clickIncX, y : state.heliPos.y + clickIncY}


checkObstacleBounds = filter (\{pos : {x, y}, mWidth, mHeight, id, background } -> if (x >= 0) then true else false )


addObstacle maxLen arr = do
  i <- randomInt 1 (gameAreaHeight/2)
  if (i % 2) == 0
    then
      if ((length arr) < maxLen) then pure (arr <> [{pos : {x : (gameAreaWidth - 50), y : 0 }, mWidth : 50, mHeight : i, id : (length arr) , background : "#009900"}]) else pure arr
    else
      if ((length arr) < maxLen) then pure (arr <> [{pos : {x : (gameAreaWidth - 50), y : (gameAreaHeight - i) }, mWidth : 50, mHeight : i, id : (length arr) , background : "#009900"}]) else pure arr


frameUpdate x = do
  (state::MyState) <- U.getState
  _ <- U.updateState "heliPos" {x : state.heliPos.x + fallIncX, y : state.heliPos.y + fallIncY}
  newObs <- (addObstacle numObstaclesOnScreen state.obstacles)
  U.updateState "obstacles" $ checkObstacleBounds $ (updateObstaclePos (-2) 0)  <$> newObs

listen = do
  sigSquare <- U.signal "gameArea" ""

  let behavior = eval <$> sigSquare.behavior
  let events = sigSquare.event
  let frameBehv = frameUpdate <$> sigSquare.behavior

  _ <- U.patch widget behavior events
  U.patch widget frameBehv (animationFrame)


main = do
  --- Init State {} empty record--
  U.initializeState
  --- Update State ----
  _ <- U.updateState "background" "#888888"
  _ <- U.updateState "numObstacles" 0
  _ <- U.updateState "obstacles" getObstacles
  state <- U.updateState "heliPos" {x : heliInitX, y :heliInitY}

  ---  global  key value pair in your "state" (which is also global)
  ---- Render Widget ---
  U.render (widget state) listen
  pure unit
