module Blessed.UI.DataDisplay.Log.Event where

import Blessed.Core.Key (Key)

import Blessed.Internal.BlessedSubj (Element, Box, Log, class Extends)
import Blessed.Internal.Core (handler, Handler) as C
import Blessed.Internal.Emitter (class Fires) as C

import Blessed.UI.Base.Element.Event as E
import Blessed.UI.Boxes.Box.Event as B


type Event = B.BoxEvent


type LogEvent = B.BoxEvent



logHandler :: forall subj id r state. Extends Element subj => Extends Box subj => Extends Log subj => C.Fires subj LogEvent => LogEvent -> C.Handler subj id r state
logHandler = C.handler


key :: forall subj id r state. Extends Element subj => Extends Box subj => Extends Log subj => C.Fires subj LogEvent => Array Key -> C.Handler subj id r state
key = E.key


on :: forall subj id r state. Extends Element subj => Extends Box subj => Extends Log subj => C.Fires subj LogEvent => LogEvent -> C.Handler subj id r state
on = E.on
