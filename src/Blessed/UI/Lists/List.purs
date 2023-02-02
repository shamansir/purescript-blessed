module Blessed.UI.Lists.List
    ( list
    , listAnd
    ) where


import Type.Row (type (+))
import Data.Symbol (class IsSymbol)


import Blessed.Internal.Core (Node, NodeAnd, node, nodeAnd) as C
import Blessed.Internal.BlessedSubj (List) as Subject
import Blessed.Internal.NodeKey (NodeKey)
import Blessed.UI.Boxes.Box.Option (OptionsRow) as Box
import Blessed.UI.Lists.List.Event (Event)
import Blessed.UI.Lists.List.Option (OptionsRow)


list :: forall id r state. IsSymbol id => NodeKey Subject.List id -> C.Node Subject.List id ( Box.OptionsRow + OptionsRow + r ) state Event
list nodeKey = C.node nodeKey


listAnd :: forall id r state. IsSymbol id => NodeKey Subject.List id -> C.NodeAnd Subject.List id ( Box.OptionsRow + OptionsRow + r ) state Event
listAnd nodeKey = C.nodeAnd nodeKey