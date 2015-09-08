module Translator where
import Turtle
import qualified Data.Map.Strict as M

type Translator = (Char -> Command)

defaultMap :: M.Map Char Command
defaultMap = M.fromList $
    [ ('[', Save), (']', Load)]
    ++ [ ('{', Save), ('}', Load)]
    ++ zip ['1'..'9'] (map (Draw . fromIntegral . (* 5)) ([1..9]::[Int]))
    ++ zip ['a'..'z'] (map (Turn . (\n -> pi/n) . fromIntegral) ([1..]::[Int]))
    ++ zip ['A'..'Z'] (map (Turn . (\n -> -pi/n) . fromIntegral) ([1..]::[Int]))

defaultTranslator :: Translator
defaultTranslator c = case c `M.lookup` defaultMap of
                          Nothing -> Stay
                          (Just cmd) -> cmd
