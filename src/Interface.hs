module Interface (setUpInterface) where

import Data.Maybe

import Haste
import Haste.DOM
import Haste.Events

-- {{{ Consts
idNewRuleBefore :: ElemID
idNewRuleBefore = "newRuleBefore"
idNewRuleAfter :: ElemID
idNewRuleAfter = "newRuleAfter"
idNewRuleAdd :: ElemID
idNewRuleAdd = "newRuleAdd"
idRules :: ElemID
idRules = "rules"
-- }}}

-- {{{ Adding rules
setUpAdd :: IO HandlerInfo
setUpAdd = do
    Just btn <- elemById idNewRuleAdd
    Just table <- elemById idRules
    onEvent btn Click $ \c -> do
        Just from <- getValue =<< fromJust <$> elemById idNewRuleBefore
        Just to <- getValue =<< fromJust <$> elemById idNewRuleAfter
        writeLog $ "Adding " ++ from ++ "->" ++ to
        mkTr from to >>= appendChild table

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
    d `appendChild` b
    return d
-- }}}
-- }}}

setUpInterface :: IO ()
setUpInterface = do
    _ <- setUpAdd
    return ()

-- vim:fdm=marker
