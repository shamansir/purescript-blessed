module Blessed.UI.Lists.List.Method where

import Prelude

import Blessed.Internal.NodeKey (RawNodeKey)


data Item
    = AtIndex Int
    | Element RawNodeKey
    -- TODO: Node

-- Item : ElementId | Element | Index | String

{- TODO -}

-- Child can be an element, index, or string.


{-
addItem text:String
removeItem child:Item
pushItem child:Item
popItem -> Item
unshiftItem child:Item
shiftItem -> Item
insertItem index:Int child:Item
getItem child:Item -> Element
setItem child:Item content:String
spliceItem i:Int n:Int items:ArrayItem
clearItems
setItems items:ArrayItem
getItemIndex child:Child -> Int
select index:Int
move offset:Int
up amount:Int
down amount:Int
pick callback:Blessed
fuzzyFind context
-}