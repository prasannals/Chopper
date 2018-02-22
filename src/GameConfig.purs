module GameConfig where
import Prelude

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

invSpeed :: Int
invSpeed = 30
