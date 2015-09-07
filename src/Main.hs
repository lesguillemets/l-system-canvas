import Haste
import Haste.DOM
import Haste.Graphics.Canvas
import Turtle
import LSystem
import Translator

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
    _state = ".",
    _rule = \c -> case c of
        '.' -> "2G[[.]g.]g2m[[2.]2M][m2.]M."
        '2' -> "22"
        _ -> return c
}

main = do
    Just cnv <- getCanvasById "canv"
    render cnv (color (RGB 255 255 255) . fill $ rect (0,0) (500,500))
    drawFit cnv (RenderConfig (500,500) (20,20)) blankTurtle
        . map defaultTranslator . _state . (!!6) . iterate nextGen $ treeSystem
