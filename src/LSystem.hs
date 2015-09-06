import Haste
import Haste.DOM
import Haste.Graphics.Canvas
import Turtle

main = do
    Just cnv <- getCanvasById "canv"
    drawTurtle cnv blankTurtle [DrawTo (200,200)]
