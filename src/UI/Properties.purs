module UI.Properties where

import Prelude

import Data.Tuple (Tuple(..))

import UI.Core (AttrValue(..), Prop)


prop :: String -> String -> Prop
prop key value = Tuple key (AttrValue value)

id_ :: String -> Prop
id_ = prop "id"

textFromHtml :: String -> Prop
textFromHtml = prop "textFromHtml"


showDividers :: String -> Prop
showDividers = prop "showDividers"


dividerDrawable :: String -> Prop
dividerDrawable = prop "dividerDrawable"


tabTextColors :: String -> Prop
tabTextColors = prop "tabTextColors"


selectedTabIndicatorHeight :: String -> Prop
selectedTabIndicatorHeight = prop "selectedTabIndicatorHeight"


foreground :: String -> Prop
foreground = prop "foreground"


selectedTabIndicatorColor :: String -> Prop
selectedTabIndicatorColor = prop "selectedTabIndicatorColor"


layoutTransition :: String -> Prop
layoutTransition = prop "layoutTransition"


focusOut :: String -> Prop
focusOut = prop "focusOut"


focus :: String -> Prop
focus = prop "focus"


fillViewport :: String -> Prop
fillViewport = prop "fillViewport"


setDate :: String -> Prop
setDate = prop "setDate"


minDate :: String -> Prop
minDate = prop "minDate"


maxDate :: String -> Prop
maxDate = prop "maxDate"


clipChildren :: String -> Prop
clipChildren = prop "clipChildren"


adjustViewBounds :: String -> Prop
adjustViewBounds = prop "adjustViewBounds"


maxLines :: String -> Prop
maxLines = prop "maxLines"


singleLine :: String -> Prop
singleLine = prop "singleLine"


hardware :: String -> Prop
hardware = prop "hardware"


selected :: String -> Prop
selected = prop "selected"


curve :: String -> Prop
curve = prop "curve"


fontFamily :: String -> Prop
fontFamily = prop "fontFamily"


checked :: String -> Prop
checked = prop "checked"


backgroundDrawable :: String -> Prop
backgroundDrawable = prop "backgroundDrawable"


buttonTint :: String -> Prop
buttonTint = prop "buttonTint"


visibility :: String -> Prop
visibility = prop "visibility"


scaleType :: String -> Prop
scaleType = prop "scaleType"


progressColor :: String -> Prop
progressColor = prop "progressColor"


alpha :: String -> Prop
alpha = prop "alpha"


imageUrl :: String -> Prop
imageUrl = prop "imageUrl"


url :: String -> Prop
url = prop "url"


translationY :: String -> Prop
translationY = prop "translationY"


translationX :: String -> Prop
translationX = prop "translationX"


translationZ :: String -> Prop
translationZ = prop "translationZ"


delay :: String -> Prop
delay = prop "delay"


duration :: String -> Prop
duration = prop "duration"


pivotX :: String -> Prop
pivotX = prop "pivotX"


pivotY :: String -> Prop
pivotY = prop "pivotY"


minWidth :: String -> Prop
minWidth = prop "minWidth"


minHeight :: String -> Prop
minHeight = prop "minHeight"


maxWidth :: String -> Prop
maxWidth = prop "maxWidth"


letterSpacing :: String -> Prop
letterSpacing = prop "letterSpacing"


hint :: String -> Prop
hint = prop "hint"


inputType :: String -> Prop
inputType = prop "inputType"


inputTypeI :: String -> Prop
inputTypeI = prop "inputTypeI"


textSize :: String -> Prop
textSize = prop "textSize"


fontSize :: String -> Prop
fontSize = prop "fontSize"


textIsSelectable :: String -> Prop
textIsSelectable = prop "textIsSelectable"


fontStyle :: String -> Prop
fontStyle = prop "fontStyle"


textAllCaps :: String -> Prop
textAllCaps = prop "textAllCaps"


toast :: String -> Prop
toast = prop "toast"


scaleX :: String -> Prop
scaleX = prop "scaleX"


scaleY :: String -> Prop
scaleY = prop "scaleY"


gravity :: String -> Prop
gravity = prop "gravity"


orientation :: String -> Prop
orientation = prop "orientation"


text :: String -> Prop
text = prop "text"


width :: String -> Prop
width = prop "width"


weight :: String -> Prop
weight = prop "weight"


height :: String -> Prop
height = prop "height"


layout_gravity :: String -> Prop
layout_gravity = prop "layout_gravity"


margin :: String -> Prop
margin = prop "margin"


marginStart :: String -> Prop
marginStart = prop "marginStart"


marginEnd :: String -> Prop
marginEnd = prop "marginEnd"


padding :: String -> Prop
padding = prop "padding"


cornerRadius :: String -> Prop
cornerRadius = prop "cornerRadius"


stroke :: String -> Prop
stroke = prop "stroke"


typeface :: String -> Prop
typeface = prop "typeface"


background :: String -> Prop
background = prop "background"


backgroundColor :: String -> Prop
backgroundColor = prop "backgroundColor"


color :: String -> Prop
color = prop "color"


hintColor :: String -> Prop
hintColor = prop "hintColor"


btnBackground :: String -> Prop
btnBackground = prop "btnBackground"


colorFilter :: String -> Prop
colorFilter = prop "colorFilter"


btnColor :: String -> Prop
btnColor = prop "btnColor"


shadowLayer :: String -> Prop
shadowLayer = prop "shadowLayer"


elevation :: String -> Prop
elevation = prop "elevation"


rotationX :: String -> Prop
rotationX = prop "rotationX"


rotationY :: String -> Prop
rotationY = prop "rotationY"


rotation :: String -> Prop
rotation = prop "rotation"


backgroundTint :: String -> Prop
backgroundTint = prop "backgroundTint"


scrollBarX :: String -> Prop
scrollBarX = prop "scrollBarX"


scrollBarY :: String -> Prop
scrollBarY = prop "scrollBarY"


clickable :: String -> Prop
clickable = prop "clickable"


focusable :: String -> Prop
focusable = prop "focusable"


selectable :: String -> Prop
selectable = prop "selectable"


selectableItem :: String -> Prop
selectableItem = prop "selectableItem"


values :: String -> Prop
values = prop "values"


maxSeek :: String -> Prop
maxSeek = prop "maxSeek"


accessibilityHint :: String -> Prop
accessibilityHint = prop "accessibilityHint"
