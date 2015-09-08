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
    now >>= writeLog . show
    writeLog "setting up.."
    _ <- setUpInterface
    now >>= writeLog . show
