import Haste
import Haste.DOM
import Haste.Performance
import Haste.Graphics.Canvas
import Turtle
import LSystem
import Translator
import Presets
import Interface


main = do
    _ <- setUpInterface
    Just cnv <- getCanvasById "canv"
    render cnv (color (RGB 255 255 255) . fill $ rect (0,0) (500,500))
    t0 <- now
    drawFit cnv (RenderConfig (500,500) (20,20)) blankTurtle
        . map sierpinskiHexTranslator . _state . (`nthGen` 6) $ sierpinskiHex
    t1 <- now
    writeLog $ "Took " ++ show (t1 - t0) ++ " ms to calc and render."
