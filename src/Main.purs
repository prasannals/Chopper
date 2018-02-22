module Main where

import Collision (Rect, anyOverlapping, showRect)
import Data.Array (filter, foldl, length)
import Halogen.VDom.Types (VDom)
import Prelude (bind, discard, map, negate, not, pure, show, unit, ($), (*), (+), (-), (/), (<), (<$>), (<>), (==), (>), (>=), (||))
import UI.Elements (frameLayout, imageView, textView)
import UI.Events (onClick)
import UI.Properties (background, gravity, height, id_, imageUrl, margin, text, textSize, width)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Random (randomInt, RANDOM)
import Control.Plus ((<|>))
import Data.Unit (Unit)
import DOM (DOM)
import FRP (FRP)
import FRP.Behavior.Keyboard (key)
import FRP.Event.Keyboard as K
import FRP.Event.Time (animationFrame)
import Neon (min)
import Neon.Operator ((%))
import UI.Core (Attr, AttrValue(Some), MEvent)

import UI.Util as U
import GameConfig

foreign import click :: MEvent
foreign import change :: MEvent


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

withMargin :: Rect -> Int -> Rect
withMargin {x,y,w,h} margin = {x : (x + margin),y : y + margin,w : w - margin, h:  h - margin}

getScreenSpeed :: Int -> Int
getScreenSpeed score = min (-2) (-(score / invSpeed))


heliJump :: forall t33 t34. Eff t33 { | t34 }
heliJump = do
      (state::MyState) <- U.getState
      U.updateState "isPressed" true

eval :: forall t63 t64 t65. t63 -> Eff t65 { | t64 }
eval x = heliJump

checkObstacleBounds :: forall t115 t118 t120 t121 t122 t123 t124.
  Array
    { background :: t124
    , id :: t123
    , rect :: { h :: t122
              , w :: t121
              , y :: t120
              , x :: Int
              | t118
              }
    | t115
    }
  -> Array
       { background :: t124
       , id :: t123
       , rect :: { h :: t122
                 , w :: t121
                 , y :: t120
                 , x :: Int
                 | t118
                 }
       | t115
       }
checkObstacleBounds = filter (\{rect : {x, y, w, h}, id, background } -> if (x >= 0) then true else false )


addObstacle :: forall t128.
  Int
  -> Array
       { rect :: { x :: Int
                 , y :: Int
                 , w :: Int
                 , h :: Int
                 }
       , id :: Int
       , background :: String
       }
     -> Eff
          ( random :: RANDOM
          | t128
          )
          (Array
             { rect :: { x :: Int
                       , y :: Int
                       , w :: Int
                       , h :: Int
                       }
             , id :: Int
             , background :: String
             }
          )
addObstacle maxLen arr = do
  i <- randomInt 1 (gameAreaHeight/2)
  if (i % 2) == 0
    then
      if ((length arr) < maxLen) then pure (arr <> [{rect : {x : (gameAreaWidth - 50), y : 0 , w: 50, h : i}, id : (length arr) , background : obstacleColor}]) else pure arr
    else
      if ((length arr) < maxLen) then pure (arr <> [{rect : {x : (gameAreaWidth - 50), y : (gameAreaHeight - i), w: 50, h : i }, id : (length arr) , background : obstacleColor}]) else pure arr



frameUpdate :: forall t149 t320.
  t149
  -> Eff
       ( random :: RANDOM
       , console :: CONSOLE
       | t320
       )
       { background :: String
       , heliRect :: { x :: Int
                     , y :: Int
                     , w :: Int
                     , h :: Int
                     }
       , obstacles :: Array
                        { rect :: { x :: Int
                                  , y :: Int
                                  , w :: Int
                                  , h :: Int
                                  }
                        , id :: Int
                        , background :: String
                        }
       , numObstacles :: Int
       , logText :: String
       , gameLife :: Int
       , score :: Int
       , isPressed :: Boolean
       }
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


evalKeyboard :: forall t58 t59 t60. t58 -> Eff t60 { | t59 }
evalKeyboard space = heliJump

evalKeyboardUp :: forall t47 t52 t53. t47 -> Eff t52 { | t53 }
evalKeyboardUp space = do
  (s :: MyState) <- U.getState
  U.updateState "isPressed" false


listen :: forall t384.
  Eff
    ( frp :: FRP
    , console :: CONSOLE
    , random :: RANDOM
    | t384
    )
    (Eff
       ( frp :: FRP
       , console :: CONSOLE
       , random :: RANDOM
       | t384
       )
       Unit
    )
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


main :: forall t453.
  Eff
    ( dom :: DOM
    , console :: CONSOLE
    , frp :: FRP
    , random :: RANDOM
    | t453
    )
    Unit
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
