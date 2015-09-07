import Haste
import Haste.DOM
import Haste.Graphics.Canvas
import Turtle

main = do
    Just cnv <- getCanvasById "canv"
    drawFit cnv (RenderConfig (500,500) (20,20)) blankTurtle
        . concat . replicate 8 $ [Turn (pi/13), Draw 20]
