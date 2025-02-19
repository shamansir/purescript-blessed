module Blessed.UI.DataDisplay.Log
    ( log
    , logAnd
    ) where


import Type.Row (type (+))
import Data.Symbol (class IsSymbol)
import Type.Proxy (Proxy(..))

import Blessed.Internal.Core (Node, NodeAnd, node, nodeAnd) as C
import Blessed.Internal.BlessedSubj (Log) as Subject
import Blessed.Internal.NodeKey (NodeKey)

import Blessed.UI.DataDisplay.Log.Option (OptionsRow)
import Blessed.UI.DataDisplay.Log.Event (LogEvent)


log :: forall id r state. IsSymbol id => NodeKey Subject.Log id -> C.Node Subject.Log id ( OptionsRow + r ) state
log nodeKey = C.node nodeKey


logAnd :: forall id r state. IsSymbol id => NodeKey Subject.Log id -> C.NodeAnd Subject.Log id ( OptionsRow + r ) state
logAnd nodeKey = C.nodeAnd ( Proxy :: _ LogEvent ) nodeKey
