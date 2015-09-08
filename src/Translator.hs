module Translator where
import Turtle
import qualified Data.Map.Strict as M

type Translator = (Char -> Command)

-- TODO : insufficient
-- * move?
-- * not having two chars for the same Command
defaultMap :: M.Map Char Command
defaultMap = M.fromList $
    [ ('[', Save), (']', Load)]
    ++ [ ('{', Save), ('}', Load)]
    ++ zip ['1'..'9'] (map (Draw . fromIntegral . (* 5)) ([1..9]::[Int]))
    ++ zip ['a'..'z'] (map (Turn . (\n -> pi/n) . fromIntegral) ([1..]::[Int]))
    ++ zip ['A'..'Z'] (map (Turn . (\n -> -pi/n) . fromIntegral) ([1..]::[Int]))

defaultTranslator :: Translator
defaultTranslator = fromMap defaultMap

fromMap :: M.Map Char Command -> Translator
fromMap m c = case c `M.lookup` m of
                Nothing -> Stay
                (Just cmd) -> cmd

withExtra :: M.Map Char Command -> Translator
withExtra = fromMap . (`M.union` defaultMap)

withExtraList :: [(Char,Command)] -> Translator
withExtraList = withExtra . M.fromList
