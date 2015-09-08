{-# LANGUAGE OverloadedStrings #-}
module Interface (setUpInterface) where
import Data.Maybe

import Haste
import Haste.DOM
import Haste.Events
import Haste.Foreign

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
    setProp c "innerText" from
    
    str <- newElem "td"
    setClass str "ruleAfter" True
    setProp str "innerText" to
    
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
    setProp b "innerText" "del"
    _ <- onEvent b Click $ \_ -> do
        row <- getParent d
        (c:str:_) <- mapM (`getProp` "innerText") =<< getChildren row
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
        writeLog "rendering"
        mapM_ (writeLog . show) rules
        writeLog $ "to the " ++ iteration ++ " th iteration from \n" ++ initState

getRules :: IO [(Char, String)]
getRules = mapM readTr . tail
    =<< (getChildren . fromJust) =<< elemById idRules

readTr :: Elem -> IO (Char, String)
readTr tr = do
    (chr:str:_) <- mapM (`getProp` "innerText") =<< getChildren tr
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

-- vim:fdm=marker
