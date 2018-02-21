module Collision where

import Prelude
import Data.Foldable (foldl)

type Rect = {x :: Int, y :: Int, w :: Int, h :: Int}


valueInRange :: Int -> Int -> Int -> Boolean
valueInRange val min max = (val >= min) && (val <= max)

-- r0.x r0.y is the top left point of the first rectangle
-- r0.h r0.w is the height and width of the first rectangle
-- r1.x r1.y is the top left point of the second rectangle
-- r1.h r1.w is the height and width of the second rectangle
isOverlapping :: Rect -> Rect -> Boolean
isOverlapping r0 r1 = ((valueInRange r0.x r1.x (r1.x + r1.w)) || (valueInRange r1.x r0.x (r0.x + r0.w))) && ((valueInRange r0.y r1.y (r1.y + r1.h)) || (valueInRange r1.y r0.y (r0.y + r0.h)))

anyOverlapping :: Rect -> (Array Rect) -> Boolean
anyOverlapping r0 arr = foldl (\acc v -> (acc || v)) false (map (isOverlapping r0) arr)

showRect :: Rect -> String
showRect {x, y, w, h} = (show x) <> ", " <> (show y) <> ", " <> (show w) <> ", " <> (show h)
