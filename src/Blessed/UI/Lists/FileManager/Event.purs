module Blessed.UI.Lists.FileManager.Event where

import Prelude

import Prelude ((<<<))


import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Blessed.Core.Key (Key)
import Blessed.Core.Key as Key


import Blessed.Internal.Emitter (class Events, CoreEvent(..)) as C
import Blessed.Internal.Core (handler, Handler) as C


data Event
    = Init
    | Cd
    | File


instance events :: C.Events Event where
    initial = Init

    convert Init = "init" /\ []
    convert Cd = "cd" /\ []
    convert File = "file" /\ []

    toCore _ = C.CoreEvent
    fromCore _ = Nothing



type Handler subj id r = C.Handler subj id r Event


fmHandler :: forall subj id r. Event -> Handler subj id r
fmHandler = C.handler



on :: forall subj id r. Event -> Handler subj id r
on = fmHandler