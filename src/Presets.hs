module Presets where
import LSystem

-- examples from en.wikipedia.org/wiki/L-system

pythagoras :: LSystem Char
pythagoras = LSystem {
    _state = "1",
    _rule = \c -> case c of
        '2' -> "22"
        '1' -> "2[c1]C1"
        _ -> return c
}

-- TODO: Can't handle iteration > 5
koch :: LSystem Char
koch = LSystem {
    _state = "2",
    _rule = \c -> case c of
        '2' -> "2b2B2B2b2"
        _ -> return c
}

sierpinski :: LSystem Char
sierpinski = LSystem {
    _state = "1.cc1,cc1,",
    _rule = \c -> case c of
        '.' -> ".cc1,CC1.CC1,cc1."
        ',' -> ",1,"
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
