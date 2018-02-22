module Main where

import Collision
import Data.Array
import Data.String
import Halogen.VDom.Types
import Prelude
import Types
import UI.Elements
import UI.Events
import UI.Properties

import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (stack)
import Control.Monad.Eff.Random (randomInt)
import Control.Plus ((<|>))
import DOM.HTML.Event.ErrorEvent (lineNo)
import DOM.HTML.History (back)
import Data.Array (group)
import Data.Array (length)
import Data.Array.ST (thaw)
import FRP as F
import FRP.Behavior (behavior)
import FRP.Behavior.Keyboard (key)
import FRP.Event as E
import FRP.Event.Keyboard as K
import FRP.Event.Time (animationFrame)
import Halogen.VDom.Types (graft)
import Neon (min)
import Neon.Operator ((%))
import UI.Core (MEvent, AttrValue(..), Attr(..), Prop)
import UI.Util as U

foreign import click :: MEvent
foreign import change :: MEvent


heliInitX :: Int
heliInitX = 50

heliInitY :: Int
heliInitY = 50


clickIncY :: Int
clickIncY = -8

clickIncX :: Int
clickIncX = 0

numObstaclesOnScreen :: Int
numObstaclesOnScreen = 4

fallIncX :: Int
fallIncX = 0

fallIncY :: Int
fallIncY = 8

-- -- GREEN THEME
-- htmlBackground :: String
-- htmlBackground = "#00796B"
--
-- gameAreaBackground :: String
-- gameAreaBackground = "#B2DFDB"
--
-- obstacleColor :: String
-- obstacleColor = "#009688"

-- BROWN THEME
htmlBackground :: String
htmlBackground = "#9E9E9E"

gameAreaBackground :: String
gameAreaBackground = "#ECEFF1"

obstacleColor :: String
obstacleColor = "#424242"

gameAreaHeight :: Int
gameAreaHeight = 800

gameAreaWidth :: Int
gameAreaWidth = 1200

heliWidth :: Int
heliWidth = 150

heliHeight :: Int
heliHeight = 100

heliMargin :: Int
heliMargin = 40

type Obstacle = {rect :: Rect, id :: Int, background :: String}
type MyState = {background :: String, heliRect :: Rect, obstacles :: Array Obstacle, numObstacles :: Int, logText :: String, gameLife :: Int, score :: Int, isPressed :: Boolean}



widget :: forall a . MyState  -> VDom Attr a
widget state = frameLayout
              [ id_ "root"
              , height "match_parent"
              , width "match_parent"
              , background htmlBackground
              , gravity "top"
              ]
              ([
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
                   , margin (posToMarginStr state.heliRect.x state.heliRect.y)
                  ]
                ]) <> (drawObstacle <$> state.obstacles)
                )
              ] <> [
                  textView
                  [
                    id_ "outView"
                    , height "200"
                    , width "400"
                    , textSize "40"
                    , text (genGameMessage state.score state.gameLife)
                    , margin (posToMarginStr (gameAreaWidth + 100) 200)
                  ]
                ])

genGameMessage :: Int -> Int -> String
genGameMessage score life = "Score : " <> (show score) <> "\n" <> (if life > 0 then "Game on!" else "Game Over")

posToMarginStr :: Int -> Int -> String
posToMarginStr x y = (show x) <> ", " <> (show y) <> " , 0, 0"


drawObstacle :: forall a. Obstacle -> VDom Attr a
drawObstacle obstacle = frameLayout [
    id_ (show $ obstacle.id),
    height (show $ obstacle.rect.h),
    width (show $ obstacle.rect.w),
    background obstacle.background,
    margin (posToMarginStr obstacle.rect.x obstacle.rect.y)
  ][]


getObstacles :: Array Obstacle
getObstacles = [{rect : {x : 200, y : 0, w: 50, h :50}, id : 0, background : obstacleColor},
                {rect : {x : 500, y : 0, w : 50, h : 300}, id : 1, background : obstacleColor},
                {rect : {x : 800, y : 0, w : 50, h : 200 }, id : 2, background : obstacleColor},
                {rect : {x : 1100, y : 0, w : 50, h : 400 }, id : 3, background : obstacleColor}]


updateObstaclePos :: Int -> Int -> Obstacle -> Obstacle
updateObstaclePos dx dy {rect : {x, y, w, h}, id, background }= {rect : {x: (x + dx), y : (y + dy), w, h}, id, background }

heliJump = do
      (state::MyState) <- U.getState
      U.updateState "isPressed" true

eval x = heliJump


checkObstacleBounds = filter (\{rect : {x, y, w, h}, id, background } -> if (x >= 0) then true else false )


addObstacle maxLen arr = do
  i <- randomInt 1 (gameAreaHeight/2)
  if (i % 2) == 0
    then
      if ((length arr) < maxLen) then pure (arr <> [{rect : {x : (gameAreaWidth - 50), y : 0 , w: 50, h : i}, id : (length arr) , background : obstacleColor}]) else pure arr
    else
      if ((length arr) < maxLen) then pure (arr <> [{rect : {x : (gameAreaWidth - 50), y : (gameAreaHeight - i), w: 50, h : i }, id : (length arr) , background : obstacleColor}]) else pure arr

invSpeed :: Int
invSpeed = 30

getScreenSpeed :: Int -> Int
getScreenSpeed score = min (-2) (-(score / invSpeed))

frameUpdate x = do
  (state::MyState) <- U.getState
  if state.gameLife > 0
    then do
      _ <- if (not state.isPressed)
       then U.updateState "heliRect" {x : (state.heliRect.x + fallIncX), y : (state.heliRect.y + fallIncY), w: state.heliRect.w, h: state.heliRect.h}
       else if state.gameLife > 0
         then U.updateState "heliRect" {x : state.heliRect.x + clickIncX, y : state.heliRect.y + clickIncY, w : state.heliRect.w, h : state.heliRect.h}
         else pure state
      newObs <- (addObstacle numObstaclesOnScreen state.obstacles)
      (u::MyState) <- U.updateState "obstacles" $ (updateObstaclePos (getScreenSpeed state.score) 0)  <$> newObs
      s <- U.updateState "score" (u.score + ((length (filter (\{rect : {x, y, w, h}, id, background } -> if (x < 0) then true else false ) u.obstacles)) * 10) )
      _ <- U.updateState "obstacles" $ checkObstacleBounds $ s.obstacles
      log $ foldl (\a s -> a <> "\n" <> s) "" ([showRect u.heliRect] <> (map showRect (map (\o -> o.rect) u.obstacles)))
      if (anyOverlapping (withMargin (state.heliRect) heliMargin ) (map (\o -> o.rect) state.obstacles)) || (u.heliRect.y > (gameAreaHeight - u.heliRect.h)) || (u.heliRect.y < 0)
        then U.updateState "gameLife" (state.gameLife - 1)
        else pure u
    else pure state

withMargin :: Rect -> Int -> Rect
withMargin {x,y,w,h} margin = {x : (x + margin),y : y + margin,w : w - margin, h:  h - margin}


evalKeyboard space = heliJump

evalKeyboardUp space = do
  (s :: MyState) <- U.getState
  U.updateState "isPressed" false

listen = do
  sigSquare <- U.signal "gameArea" ""

  let behavior = eval <$> sigSquare.behavior
  let events = sigSquare.event
  let frameBehv = frameUpdate <$> sigSquare.behavior
  let kBehv = evalKeyboard <$> (key 32)
  let kBehvUp = evalKeyboardUp <$> (key 32)

  _ <- U.patch widget kBehv K.down
  _ <- U.patch widget kBehvUp K.up
  _ <- U.patch widget behavior events
  U.patch widget frameBehv (animationFrame)


main = do
  --- Init State {} empty record--
  U.initializeState
  --- Update State ----
  _ <- U.updateState "background" gameAreaBackground
  _ <- U.updateState "numObstacles" 0
  _ <- U.updateState "gameLife" 1
  _ <- U.updateState "obstacles" getObstacles
  _ <- U.updateState "score" 0
  _ <- U.updateState "isPressed" false
  _ <- U.updateState "heliRect" {x : heliInitX, y :heliInitY, w : heliWidth, h : heliHeight}
  state <- U.updateState "logText" ""


  ---  global  key value pair in your "state" (which is also global)
  ---- Render Widget ---
  U.render (widget state) listen
  pure unit
