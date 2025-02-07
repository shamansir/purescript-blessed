module Blessed.Demo where

import Prelude


import Effect (Effect)

-- import Data.Text.Output.Blessed (multiLine) as Blessed

import Blessed ((>~))
import Blessed as B
import Blessed (run, exit) as Blessed

import Blessed.Internal.Core as Core
import Blessed.Internal.BlessedSubj (Screen, Box)
import Blessed.Internal.NodeKey (nk, type (<^>))

import Blessed.Core.Key (alpha, control, escape, enter) as Key
import Blessed.Core.Dimension (percents) as Dimension
import Blessed.Core.Offset (center) as Offset
import Blessed.Core.Border (type_, _line, fg, bg) as Border
import Blessed.Core.Style (fg, bg, border, hover) as Style
import Blessed.Core.EndStyle as ES

import Blessed.UI.Base.Screen.Event (key) as Screen
import Blessed.UI.Base.Screen.Method (render) as Screen
import Blessed.UI.Base.Screen.Option (fullUnicode, smartCSR, title) as Screen
import Blessed.UI.Boxes.Box.Method (focus, insertLine, setContent, setLine) as Box
import Blessed.UI.Boxes.Box.Option (border, content, height, left, style, tags, top, width) as Box
import Blessed.UI.Base.Element.Event (ElementEvent(..), key) as Element



type State = Unit


mainScreen = nk :: Screen <^> "main-scr"
theBox = nk :: Box <^> "demo-box"


main :: Effect Unit
main = demo


demo :: Effect Unit
demo = Blessed.run unit screen


screen :: Core.Blessed State
screen =
    B.screenAnd mainScreen
        [ Screen.title "my window title"
        , Screen.smartCSR true
        , Screen.fullUnicode true
        , Screen.key
            [ Key.escape, Key.alpha 'q', (Key.control $ Key.alpha 'C') ]
            $ \_ kevt -> do
                Blessed.exit
        ]
        [ box
        ]
        $ \_ -> do
            theBox >~ Box.focus
            mainScreen >~ Screen.render



box :: Core.Blessed State
box =
    B.box theBox
        [ Box.top    $ Offset.center -- Offset.calc $ Coord.center <+> Coord.px 1
        , Box.left   $ Offset.center
        , Box.width  $ Dimension.percents 50.0
        , Box.height $ Dimension.percents 50.0
        , Box.content "Hello {bold}world{/bold}!"
        , Box.tags true
        , Box.border
            [ Border.type_ Border._line
            ]
        , Box.style
            [ Style.fg "white"
            , Style.bg "magenta"
            , Style.border
                [ Border.fg "#f0f0f0"
                ]
            , Style.hover
                [ ES.bg "green"
                ]
            ]
        , Core.on Element.Click $ \_ _ -> do
            theBox >~ Box.setContent "{center}Some different {red-fg}content{/red-fg}.{/center}"
            mainScreen >~ Screen.render
        , Element.key [ Key.enter ] \_ _ -> do
            theBox >~ Box.setContent "{right}Even different {black-fg}content{/black-fg}.{/right}\n"
            theBox >~ Box.setLine 1 "bar"
            theBox >~ Box.insertLine 1 "foo"
            mainScreen >~ Screen.render
        ]
        []