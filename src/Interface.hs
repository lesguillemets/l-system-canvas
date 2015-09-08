{-# LANGUAGE OverloadedStrings #-}
module Interface (setUpInterface) where
import Data.Maybe

import Haste
import Haste.DOM
import Haste.Events
import Haste.Foreign

-- {{{ Consts
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
-- }}}

-- {{{ Adding rules
setUpAdd :: IO HandlerInfo
setUpAdd = do
    Just btn <- elemById idNewRuleAdd
    Just table <- elemById idRules
    onEvent btn Click $ \c -> do
        Just ruleBefore <- elemById idNewRuleBefore
        Just ruleAfter <- elemById idNewRuleAfter
        Just from <- getValue ruleBefore
        Just to <- getValue ruleAfter
        writeLog $ "Adding " ++ from ++ "->" ++ to
        mkTr from to >>= appendChild table
        setProp ruleBefore "value" ""
        setProp ruleAfter "value" ""

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
        mapM_ (writeLog . show) rules

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
