module Blessed.UI.Screen.Getters where

import Prelude

import Prim.Row as R
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Maybe (Maybe(..))

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC

import Blessed.Internal.Core (NodeId) as C
import Blessed.Internal.BlessedOp as Op
import Blessed.Internal.Command (get) as C


-- newtype Focused = Focused String


type PropertiesRow =
    -- program ::
    -- tput ::
    ( focused :: Maybe String -- ?
    , width :: Int
    , height :: Int
    , cols :: Int
    , rows :: Int
    , left :: Int
    , right :: Int
    , top :: Int
    , bottom :: Int
    , aleft :: Int
    , aright :: Int
    , atop :: Int
    , abottom :: Int
    , grabKeys :: Boolean
    , lockKeys :: Boolean
    , hover :: Maybe String -- ?
    , terminal :: String
    , title :: String
    )


type Getter m a = Op.BlessedOpG m a


getter :: forall sym r' (r :: Row Type) m a. IsSymbol sym => R.Cons sym a r' PropertiesRow => Proxy sym -> CA.JsonCodec a -> C.NodeId -> Getter m a
getter sym codec nodeId =
    Op.performGet codec nodeId $ C.get $ reflectSymbol sym


focused :: forall m. C.NodeId -> Getter m (Maybe String)
focused = getter (Proxy :: _ "focused") (CAC.maybe CA.string)


width :: forall m. C.NodeId -> Getter m Int
width = getter (Proxy :: _ "width") CA.int


height :: forall m. C.NodeId -> Getter m Int
height = getter (Proxy :: _ "height") CA.int


cols :: forall m. C.NodeId -> Getter m Int
cols = getter (Proxy :: _ "cols") CA.int


rows :: forall m. C.NodeId -> Getter m Int
rows = getter (Proxy :: _ "rows") CA.int


left :: forall m. C.NodeId -> Getter m Int
left = getter (Proxy :: _ "left") CA.int


right :: forall m. C.NodeId -> Getter m Int
right = getter (Proxy :: _ "right") CA.int


top :: forall m. C.NodeId -> Getter m Int
top = getter (Proxy :: _ "top") CA.int


bottom :: forall m. C.NodeId -> Getter m Int
bottom = getter (Proxy :: _ "bottom") CA.int


aleft :: forall m. C.NodeId -> Getter m Int
aleft = getter (Proxy :: _ "aleft") CA.int


aright :: forall m. C.NodeId -> Getter m Int
aright = getter (Proxy :: _ "aright") CA.int


atop :: forall m. C.NodeId -> Getter m Int
atop = getter (Proxy :: _ "atop") CA.int


abottom :: forall m. C.NodeId -> Getter m Int
abottom = getter (Proxy :: _ "abottom") CA.int


grabKeys :: forall m. C.NodeId -> Getter m Boolean
grabKeys = getter (Proxy :: _ "grabKeys") CA.boolean


lockKeys :: forall m. C.NodeId -> Getter m Boolean
lockKeys = getter (Proxy :: _ "lockKeys") CA.boolean


hover :: forall m. C.NodeId -> Getter m (Maybe String)
hover = getter (Proxy :: _ "hover") (CAC.maybe CA.string)


terminal :: forall m. C.NodeId -> Getter m String
terminal = getter (Proxy :: _ "terminal") CA.string


title :: forall m. C.NodeId -> Getter m String
title = getter (Proxy :: _ "title") CA.string
