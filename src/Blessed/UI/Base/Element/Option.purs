module Blessed.UI.Base.Element.Option where

import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Blessed.Core.Dimension (Dimension)
import Blessed.Core.Offset (Offset)
import Blessed.Core.Color (Color)
import Blessed.Core.Padding (Padding)
import Blessed.Core.Flex (Flex)
import Blessed.Core.Align (HAlign, VAlign)
import Blessed.Core.Style (StyleOption)
import Blessed.Core.Border (BorderOption)

import Blessed.Internal.Core (Attribute, option) as C
import Blessed.Internal.BlessedSubj (Subject, Element)
import Blessed.Internal.NodeKey (class Respresents)

import Blessed.UI.Boxes.Box.Event (Event)


type OptionsRow r =
    ( fg :: Color
    , bg :: Color
    , bold :: Boolean
    , underline :: Boolean
    , style :: Array (StyleOption ())
    , border :: Array (BorderOption ())
    , content :: String -- a ?
    , clickable :: Boolean
    , input :: Boolean
    , keyable :: Boolean
    , focused :: Boolean
    , hidden :: Boolean
    , label :: String
    , hoverText :: String
    , align :: HAlign
    , valign :: VAlign
    , shrink :: Flex
    , padding :: Padding
    , width :: Dimension
    , height :: Dimension
    , left :: Offset
    , right :: Offset
    , top :: Offset
    , bottom :: Offset
    -- , position ::
    , scrollable :: Boolean
    , ch :: Char
    , draggable :: Boolean
    , shadow :: Boolean


    , tags :: Boolean
    , hover :: Array (StyleOption ())
    | r
    )
type Options = Record (OptionsRow ())


type ElementAttribute subj id r e = C.Attribute subj id (OptionsRow + r) e


elmOption
    :: forall subj id a r r' sym e
     . Respresents Element subj id
    => EncodeJson a
    => IsSymbol sym
    => R.Cons sym a r' (OptionsRow + r)
    => Proxy sym -> a -> ElementAttribute subj id r e
elmOption = C.option


fg
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => Color -> ElementAttribute subj id ( fg :: Color | r ) e
fg = elmOption (Proxy :: _ "fg")


bg
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => Color -> ElementAttribute subj id ( bg :: Color | r ) e
bg = elmOption (Proxy :: _ "bg")


bold
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => Boolean -> ElementAttribute subj id ( bold :: Boolean | r ) e
bold = elmOption (Proxy :: _ "bold")


underline
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => Boolean -> ElementAttribute subj id ( underline :: Boolean | r ) e
underline = elmOption (Proxy :: _ "underline")


style
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => Array (StyleOption ()) -> ElementAttribute subj id ( style :: Array (StyleOption ()) | r ) e
style = elmOption (Proxy :: _ "style")


border
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => Array (BorderOption ()) -> ElementAttribute subj id ( border :: Array (BorderOption ()) | r ) e
border = elmOption (Proxy :: _ "border")


content
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => String -> ElementAttribute subj id ( content :: String | r ) e
content = elmOption (Proxy :: _ "content")


clickable
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => Boolean -> ElementAttribute subj id ( clickable :: Boolean | r ) e
clickable = elmOption (Proxy :: _ "clickable")


input
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => Boolean -> ElementAttribute subj id ( input :: Boolean | r ) e
input = elmOption (Proxy :: _ "input")


keyable
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => Boolean -> ElementAttribute subj id ( keyable :: Boolean | r ) e
keyable = elmOption (Proxy :: _ "keyable")


focused
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => Boolean -> ElementAttribute subj id ( focused :: Boolean | r ) e
focused = elmOption (Proxy :: _ "focused")


hidden
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => Boolean -> ElementAttribute subj id ( hidden :: Boolean | r ) e
hidden = elmOption (Proxy :: _ "hidden")


label
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => String -> ElementAttribute subj id ( label :: String | r ) e
label = elmOption (Proxy :: _ "label")


hoverText
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => String -> ElementAttribute subj id ( hoverText :: String | r ) e
hoverText = elmOption (Proxy :: _ "hoverText")


align
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => HAlign -> ElementAttribute subj id ( align :: HAlign | r ) e
align = elmOption (Proxy :: _ "align")


valign
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => VAlign -> ElementAttribute subj id ( valign :: VAlign | r ) e
valign = elmOption (Proxy :: _ "valign")


shrink
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => Flex -> ElementAttribute subj id ( shrink :: Flex | r ) e
shrink = elmOption (Proxy :: _ "shrink")


padding
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => Padding -> ElementAttribute subj id ( padding :: Padding | r ) e
padding = elmOption (Proxy :: _ "padding")


width
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => Dimension -> ElementAttribute subj id ( width :: Dimension | r ) e
width = elmOption (Proxy :: _ "width")


height
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => Dimension -> ElementAttribute subj id ( height :: Dimension | r ) e
height = elmOption (Proxy :: _ "height")


left
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => Offset -> ElementAttribute subj id ( left :: Offset | r ) e
left = elmOption (Proxy :: _ "left")


right
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => Offset -> ElementAttribute subj id ( right :: Offset | r ) e
right = elmOption (Proxy :: _ "right")


top
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => Offset -> ElementAttribute subj id ( top :: Offset | r ) e
top = elmOption (Proxy :: _ "top")


bottom
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => Offset -> ElementAttribute subj id ( bottom :: Offset | r ) e
bottom = elmOption (Proxy :: _ "bottom")


ch
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => Char -> ElementAttribute subj id ( ch :: Char | r ) e
ch = elmOption (Proxy :: _ "ch")


draggable
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => Boolean -> ElementAttribute subj id ( draggable :: Boolean | r ) e
draggable = elmOption (Proxy :: _ "draggable")


shadow
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => Boolean -> ElementAttribute subj id ( shadow :: Boolean | r ) e
shadow = elmOption (Proxy :: _ "shadow")


tags
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Element subj id
    => Boolean -> ElementAttribute subj id ( tags :: Boolean | r ) e
tags = elmOption (Proxy :: _ "tags")
