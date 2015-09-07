import Haste
import Haste.DOM
import Haste.Graphics.Canvas
import Turtle
import LSystem

toCommands :: String -> [Command]
toCommands = map (\c -> case c of
    'X' -> Stay
    '+' -> Turn (pi/7)
    '-' -> Turn (-pi/7)
    '*' -> Turn (pi/13)
    '/' -> Turn (-pi/13)
    '[' -> Save
    ']' -> Load
    'F' -> Draw 10
    )
treeSystem = LSystem {
    _state = "X",
    _rule = \c -> case c of
        'X' -> "F-[[X]+X]+F*[[FX]F/][*FX]/X"
        'F' -> "FF"
        _ -> return c
}

main = do
    Just cnv <- getCanvasById "canv"
    render cnv (color (RGB 255 255 255) . fill $ rect (0,0) (500,500))
    drawFit cnv (RenderConfig (500,500) (20,20)) blankTurtle
        . toCommands . _state . (!!5) . iterate nextGen $ treeSystem
