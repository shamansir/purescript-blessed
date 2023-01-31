module Blessed.UI.Special.Terminal.Option where

import Prelude



import Type.Row (type (+))
import Prim.Row as R
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (class EncodeJson)
import Type.Proxy (Proxy(..))
import Data.Symbol (class IsSymbol)


import Blessed.Core.FgBg (FgBgOption)
import Blessed.Core.Cursor (Cursor)
import Blessed.Core.Terminal (Terminal)

import Blessed.Internal.Core (Attribute, option) as C
import Blessed.Internal.BlessedSubj (Subject)
import Blessed.Internal.BlessedSubj (Terminal) as Subj
import Blessed.Internal.NodeKey (class Respresents)


import Blessed.UI.Lists.List.Event (Event)
import Blessed.UI.Lists.List.Option (OptionsRow) as List


type OptionsRow r =
    ( handler :: Unit -- TODO
    , shell :: String
    , args :: Array Json
    , cursor :: Cursor
    , terminal :: Terminal
    | r
    )
type Options = Record (OptionsRow ())


type TerminalAttribute subj id r e = C.Attribute subj id (List.OptionsRow + OptionsRow + r) e


terminalOption
    :: forall subj id a r r' sym e
     . Respresents Subj.Terminal subj id
    => EncodeJson a
    => IsSymbol sym
    => R.Cons sym a r' (OptionsRow + r)
    => Proxy sym -> a -> TerminalAttribute subj id r e
terminalOption = C.option


handler
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Subj.Terminal subj id
    => Unit -> TerminalAttribute subj id ( handler :: Unit | r ) e
handler = terminalOption (Proxy :: _ "handler")


shell
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Subj.Terminal subj id
    => String -> TerminalAttribute subj id ( shell :: String | r ) e
shell = terminalOption (Proxy :: _ "shell")


args
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Subj.Terminal subj id
    => Array Json -> TerminalAttribute subj id ( args :: Array Json | r ) e
args = terminalOption (Proxy :: _ "args")


cursor
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Subj.Terminal subj id
    => Cursor -> TerminalAttribute subj id ( cursor :: Cursor | r ) e
cursor = terminalOption (Proxy :: _ "cursor")


terminal
    :: forall (subj :: Subject) (id :: Symbol) r e
     . Respresents Subj.Terminal subj id
    => Terminal -> TerminalAttribute subj id ( terminal :: Terminal | r ) e
terminal = terminalOption (Proxy :: _ "terminal")
