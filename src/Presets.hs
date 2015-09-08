module Presets where
import LSystem

pythagoras :: LSystem Char
pythagoras = LSystem {
    _state = "1",
    _rule = \c -> case c of
        '2' -> "22"
        '1' -> "2[c1]C1"
        _ -> return c
}

treeB :: LSystem Char
treeB = LSystem {
    _state = ".",
    _rule = \c -> case c of
        '.' -> "2G[[.]g.]g2m[[2.]2M][m2.]M."
        '2' -> "22"
        _ -> return c
}
