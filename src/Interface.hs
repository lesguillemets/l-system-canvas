{-# LANGUAGE OverloadedStrings #-}
module Interface (setUpInterface) where
import Data.Maybe

import Haste
import Haste.DOM
import Haste.Graphics.Canvas
import Haste.Events
import Haste.Foreign
import Haste.Performance

import LSystem
import Translator
import Turtle

-- {{{ Consts
showWarn :: String -> IO ()
showWarn = alert

idNewRuleBefore :: ElemID
idNewRuleBefore = "newRuleBefore"
idNewRuleAfter :: ElemID
idNewRuleAfter = "newRuleAfter"
idNewRuleAdd :: ElemID
idNewRuleAdd = "newRuleAdd"
idRules :: ElemID
idRules = "rules"
idDraw :: ElemID
idDraw = "draw"
idInit :: ElemID
idInit = "init"
idIter :: ElemID
idIter = "iter"
idCanv :: ElemID
idCanv = "canv"
-- }}}

-- {{{ Adding rules
setUpAdd :: IO HandlerInfo
setUpAdd = do
    Just btn <- elemById idNewRuleAdd
    Just table <- elemById idRules
    onEvent btn Click $ \_ -> do
        Just ruleBefore <- elemById idNewRuleBefore
        Just ruleAfter <- elemById idNewRuleAfter
        Just from <- getValue ruleBefore
        Just to <- getValue ruleAfter
        case from of
            [c] -> do
                writeLog $ "Adding " ++ c:"->" ++ to
                mkTr [c] to >>= appendChild table
                setProp ruleBefore "value" ""
                setProp ruleAfter "value" ""
            _ -> do
                writeLog $ "Got \"" ++ from ++ "\", unexpected length."
                showWarn "Not a valid length!"

-- {{{ Making row
mkTr :: String -> String -> IO Elem
mkTr from to = do
    tr <- newElem "tr"
    c <- newElem "td"
    setClass c "char" True
    setProp c "textContent" from
    
    str <- newElem "td"
    setClass str "ruleAfter" True
    setProp str "textContent" to
    
    d <- del
    
    tr `setChildren` [c,str,d]
    return tr
-- }}}

-- {{{ Creating delete button
del :: IO Elem
del = do
    d <- newElem "td"
    setClass d "del" True
    b <- newElem "div"
    setClass b "button" True
    setProp b "textContent" "del"
    _ <- onEvent b Click $ \_ -> do
        row <- getParent d
        (c:str:_) <- mapM (`getProp` "textContent") =<< getChildren row
        writeLog $ "Deleting " ++ c ++ " -> " ++ str
        getParent row >>= (`deleteChild` row)
    d `appendChild` b
    return d
-- }}}
-- }}}

setUpDraw :: IO HandlerInfo
setUpDraw = do
    Just button <- elemById idDraw
    onEvent button Click $ \_ -> do
        rules <- getRules
        Just initState <- getValue . fromJust =<< elemById idInit
        Just iteration <- getValue . fromJust =<< elemById idIter
        drawWith (reverse rules) initState (read iteration)

getRules :: IO [(Char, String)]
getRules = mapM readTr . tail
    =<< (getChildren . fromJust) =<< elemById idRules

readTr :: Elem -> IO (Char, String)
readTr tr = do
    (chr:str:_) <- mapM (`getProp` "textContent") =<< getChildren tr
    return (head chr, str)

setUpInterface :: IO [HandlerInfo]
setUpInterface = sequence [
    setUpAdd,
    setUpDraw
    ]

-- helper {{{
getParent :: Elem -> IO Elem
getParent = ffi "(function(x) {return x.parentNode;})"
-- }}}

drawWith :: [(Char,String)] -- rules
         -> String          --initial state
         -> Int             -- iteration
         -> IO ()
drawWith rules initial n = do
    Just cnv <- getCanvasById idCanv
    render cnv (color (RGB 255 255 255) . fill $ rect (0,0) (500,500))
    t0 <- now
    let syst = LSystem initial (ruleFromList rules)
    drawFitRainbow cnv (RenderConfig (500,500) (20,20)) blankTurtle
        . map defaultTranslator . _state . (`nthGen` n) $ syst
    t1 <- now
    writeLog $ "Took " ++ show (t1 - t0) ++ " ms to calc and render."
-- vim:fdm=marker
