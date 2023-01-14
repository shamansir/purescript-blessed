module Blessed.UI.Screen where

import Prelude


import Type.Row (type (+))
import Prim.Row (class Union, class Nub)
import Record as Record
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Data.Codec.Argonaut as CA
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)
import Data.Maybe (Maybe(..))


import Blessed.Internal.Command (Command, NodeId, call, arg) as C
import Blessed.Internal.Core (Prop, prop, Node, NodeAnd, node, nodeAnd, class Events, CoreEvent(..), handler) as C
import Blessed.Internal.BlessedOp (BlessedOp)
import Blessed.Internal.BlessedOp (perform) as Op
import Blessed.UI.Box as Box


import Data.Symbol (reflectSymbol, class IsSymbol)


data Event
    = Init
    | Key Unit
    | Other


instance events :: C.Events Event where
    initial = Init
    convert Init = "init"
    convert (Key key) = "key"
    convert Other = "?"
    toCore _ = C.CoreEvent
    fromCore _ = Nothing


type OptionsRow r =
    ( title :: String
    , smartCSR :: Boolean
    | Box.OptionsRow + r
    )
type OptionsU = OptionsRow ()
type Options = Record (OptionsU)


default :: Options
default =
    Record.merge
        { title : ""
        , smartCSR : false
        }
        Box.default


define ∷ forall (r ∷ Row Type)
    . Union r (OptionsRow ()) (OptionsRow ())
    ⇒ Nub r ((OptionsRow ()))
    ⇒ Record r → Options
define rec =
    Record.merge rec default


screenProp :: forall a r r' sym m. EncodeJson a => IsSymbol sym => R.Cons sym a r' r => Proxy sym -> a -> C.Prop (OptionsRow + r) m Event
screenProp = C.prop


screenHandler :: forall m r. Event -> BlessedOp m -> C.Prop (OptionsRow + r) m Event
screenHandler = C.handler


--draggable :: forall r e. Boolean -> Prop ( draggable :: Boolean | r ) e

title ∷ forall r m. String -> C.Prop ( title :: String | OptionsRow + r ) m Event
title = screenProp ( Proxy :: Proxy "title" )


smartCSR ∷ forall r m. Boolean -> C.Prop ( smartCSR :: Boolean | OptionsRow + r ) m Event
smartCSR = screenProp ( Proxy :: Proxy "smartCSR" )


screen :: forall r m. String -> C.Node ( OptionsRow + r ) m Event
screen name = C.node name


screenAnd :: forall r m. String -> C.NodeAnd ( OptionsRow + r ) m Event
screenAnd name = C.nodeAnd name


key :: forall r m. String -> BlessedOp m -> C.Prop (OptionsRow + r) m Event
key _ =
    screenHandler (Key unit)


render :: forall m. C.NodeId -> BlessedOp m
render nodeId =
    Op.perform nodeId
        $ C.call nodeId "render" [ ]
