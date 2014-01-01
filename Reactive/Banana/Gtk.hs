{-
Reacrive Banana Gtk is free software: you can redistribute it and/or modify
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

{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             ExistentialQuantification,
             FunctionalDependencies  #-}

module Reactive.Banana.Gtk (
  Attribute (..), EventS (..),
  registerEvent, event, behavior, sink, onEvent, whenEvent, closeGtk
  ) where

import Graphics.UI.Gtk
import qualified Reactive.Banana as B
import Reactive.Banana.Frameworks
import Control.Monad.Reader
import Foreign.Ptr

-- | Used in sink to combine a glib attribute with a reactive-banana behavior
data Attribute t o = forall a. Attr o a :=: B.Behavior t a
infixr 0 :=:

-- | Wrapper to use an EventM Gtk event with stopEvent
data EventS t a = EventS {getEventM :: EventM t a}

class Event e r | e -> r where
  eventFunction :: (r -> IO ()) -> e

class ConstEvent e r where
  constFunction :: IO r -> (r -> IO ()) -> e

registerEventGeneric :: Frameworks t => ((r -> IO ()) -> e) -> o ->
                        Signal o e -> B.Moment t (AddHandler r)
registerEventGeneric f object callback = do
  (addHandler, runHandlers) <- liftIO newAddHandler
  _ <- liftIO $ object `on` callback $ f runHandlers
  return addHandler

-- | Return an addHandler for a Gtk event
registerEvent :: (Event e r, Frameworks t) =>
                 o -> Signal o e -> B.Moment t (AddHandler r)
registerEvent = registerEventGeneric eventFunction

-- | Return a banana Event for a Gtk event
event :: (Event e r, Frameworks t) =>
          o -> Signal o e -> B.Moment t (B.Event t r)
event object evt = registerEvent object evt >>= fromAddHandler

-- | Return a behavior with a glib value triggered by a Gtk value
behavior :: (ConstEvent e r, Frameworks t) => o -> Attr o r -> Signal o e ->
            B.Moment t (B.Behavior t r)
behavior object attribute signal = do
  initialValue <- liftIO $ get object attribute
  v <- registerEventGeneric (constFunction $ get object attribute) object signal
  fromChanges initialValue v

-- | Change various properties in a widget.
sink :: Frameworks t => o -> [Attribute t o] -> B.Moment t ()
sink object = mapM_ setAttribute
  where setAttribute (attribute :=: value) = do
          initialValue <- initial value
          liftIOLater $ setup initialValue
          changedValue <- changes value
          reactimate  $ setup B.<$> changedValue
            where setup val = set object [attribute := val]

-- | Execute an IO Action when an event trig.
onEvent :: Frameworks t => B.Event t a -> (a -> IO ()) -> B.Moment t ()
onEvent evt action = reactimate $ action B.<$> evt

-- | Simply flip onEvent
whenEvent :: Frameworks t => (a -> IO ()) -> B.Event t a -> B.Moment t ()
whenEvent = flip onEvent

-- | An helper for const mainQuit, directly usable with onEvent or whenEvent
closeGtk :: a -> IO ()
closeGtk = const mainQuit

instance Event (EventM a Bool) (Ptr a) where
  eventFunction runHandlers       = tryEvent $ ask >>= liftIO . runHandlers

instance ConstEvent (EventM a Bool) b where
  constFunction value runHandlers = tryEvent . liftIO $ value >>= runHandlers

instance Event (EventS a Bool) (Ptr a) where
  eventFunction runHandlers =
    EventS $ tryEvent $ ask >>= liftIO . runHandlers >> stopEvent
    
instance ConstEvent (EventS a Bool) b where
  constFunction value runHandlers =
    EventS $ tryEvent $ liftIO (value >>= runHandlers) >> stopEvent

instance Event (IO ()) () where
  eventFunction = ($ ())

instance ConstEvent (IO ()) a where
  constFunction value runHandlers = value >>= runHandlers

instance Event (a -> IO ()) a where
  eventFunction = id

instance ConstEvent (a -> IO ()) b where
  constFunction value runHandlers _ = value >>= runHandlers

instance Event (a -> b -> IO ()) (a, b) where
  eventFunction = curry

instance ConstEvent (a -> b -> IO ()) c where
  constFunction value runHandlers _ _ = value >>= runHandlers

instance Event (a -> b -> c -> IO ()) (a, b, c) where
  eventFunction f a b c = f (a, b, c)

instance ConstEvent (a -> b -> c -> IO ()) d where
  constFunction value runHandlers _ _ _ = value >>= runHandlers
