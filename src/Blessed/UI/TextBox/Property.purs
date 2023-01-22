module Blessed.UI.TextBox.Property where

import Prelude

import Prim.Row as R
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Maybe (Maybe(..))

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC

import Blessed.Internal.Core as C


-- newtype Focused = Focused String


type PropertiesRow =
    ( secret :: String
    , censor :: String
    )


getter :: forall sym r' m a. R.Cons sym a r' PropertiesRow => C.GetterFn sym r' PropertiesRow m a
getter =
    C.getter


censor :: forall m. C.NodeId -> C.Getter m String
censor = getter (Proxy :: _ "censor") CA.string


secret :: forall m. C.NodeId -> C.Getter m String
secret = getter (Proxy :: _ "secret") CA.string
