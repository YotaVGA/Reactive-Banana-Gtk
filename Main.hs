{-
Reacrive Vanana Gtk is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Reactive Banana Gtk is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Reactive Banana Gtk.  If not, see <http://www.gnu.org/licenses/>.
-}

-- TODO: Complete the example!

module Main where

import Graphics.UI.Gtk
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Gtk
import Text.Read

data Fields = Fields Integer String String

startingFields :: Fields
startingFields = Fields 0 "" ""

updateField :: Int -> String -> Fields -> Fields
updateField i v (Fields n a b)
  | n > 0     = Fields (n - 1) a b
  | i == 0    = Fields 1 v b
  | otherwise = Fields 1 a v

convert :: Float -> String -> String
convert factor value = convert' (readMaybe value)
  where convert' Nothing       = "0"
        convert' (Just number) = show $ number * factor

main :: IO ()
main = do
  _ <- initGUI
  builder <- builderNew
  builderAddFromFile builder "gui.glade"
  
  wWindow  <- builderGetObject builder castToWindow "window"
  wValue1  <- builderGetObject builder castToEntry  "value1"
  wValue2  <- builderGetObject builder castToEntry  "value2"
  
  widgetShowAll wWindow

  network <- compile $ do
    eDelete <- event    wWindow deleteEvent
    bValue1 <- behavior wValue1 entryText editableChanged

    sink wValue2 [entryText :=: convert 1.3 <$> bValue1]

    onEvent eDelete closeGtk

  actuate network
  
  mainGUI
