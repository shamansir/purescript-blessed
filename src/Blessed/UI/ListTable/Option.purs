module Blessed.UI.ListTable.Option where

import Prelude (Unit)

import Effect (Effect)
import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Blessed.Core.Color (Color)
import Blessed.Core.Orientation (Orientation)
import Blessed.Core.Border (BorderType) as B
import Blessed.Core.FgBg (FgBgOption)
import Blessed.Core.FgBg (Evaluated) as FgBg

import Blessed.Internal.Core (Attribute, option) as C


import Blessed.UI.List.Event (Event)
import Blessed.UI.List.Option (OptionsRow) as List


type OptionsRow r =
    ( rows :: Array (Array String)
    , pad :: Int
    , noCellBorders :: Boolean
    , style_header :: Array (FgBgOption ())
    , style_cell :: Array (FgBgOption ())
    | r
    )
type Options = Record (OptionsRow ())


type ListTableAttribute r = C.Attribute (List.OptionsRow + OptionsRow + r) Event


ltOption :: forall a r r' sym. EncodeJson a => IsSymbol sym => R.Cons sym a r' (OptionsRow + r) => Proxy sym -> a -> ListTableAttribute r
ltOption = C.option


rows :: forall r. Array (Array String) -> ListTableAttribute ( rows :: Array (Array String) | r )
rows = ltOption (Proxy :: _ "rows")


pad :: forall r. Int -> ListTableAttribute ( pad :: Int | r )
pad = ltOption (Proxy :: _ "pad")


noCellBorders :: forall r. Boolean -> ListTableAttribute ( noCellBorders :: Boolean | r )
noCellBorders = ltOption (Proxy :: _ "noCellBorders")


style_header :: forall r. Array (FgBgOption ()) -> ListTableAttribute ( style_header :: Array (FgBgOption ()) | r )
style_header = ltOption (Proxy :: _ "style_header")


style_cell :: forall r. Array (FgBgOption ()) -> ListTableAttribute ( style_cell :: Array (FgBgOption ()) | r )
style_cell = ltOption (Proxy :: _ "style_cell")
