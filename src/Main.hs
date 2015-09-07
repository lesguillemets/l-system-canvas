import Haste
import Haste.DOM
import Haste.Performance
import Haste.Graphics.Canvas
import Turtle
import LSystem
import Translator

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
    t0 <- now
    drawFit cnv (RenderConfig (500,500) (20,20)) blankTurtle
        . map defaultTranslator . _state . (!!6) . iterate nextGen $ treeSystem
    t1 <- now
    writeLog $ "Took " ++ show (t1 - t0) ++ " ms to calc and render."
