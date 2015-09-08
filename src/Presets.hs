module Presets where
import LSystem
import Turtle
import Translator

-- {{{ examples from en.wikipedia.org/wiki/L-system

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

sierpinskiHex :: LSystem Char
sierpinskiHex = LSystem {
    _state = ".",
    _rule = \c ->case c of
        '.' -> "c,C.C,c"
        ',' -> "C.c,c.C"
        _ -> return c
}

sierpinskiHexTranslator :: Translator
sierpinskiHexTranslator = withExtraList [ ('.', Draw 2), (',', Draw 2)]

dragon :: LSystem Char
dragon = LSystem {
    _state = "2.",
    _rule = \c -> case c of
        '.' -> ".b,2b"
        ',' -> "B2.B,"
        _ -> return c
}

plant :: LSystem Char
plant = LSystem {
    _state = ".",
    _rule = \c -> case c of
        '.' -> "2g[[.]G.]G2[G2.]g."
        '2' -> "22"
        _ -> return c
}
-- }}}

-- from: https://www.sidefx.com/docs/houdini14.0/nodes/sop/lsystem
kochIsland :: LSystem Char
kochIsland = LSystem {
    _state = "2b2b2b2",
    _rule = \c -> case c of
        '2' -> "2b2B2B22b2b2B2"
        _ -> return c
}

plantB :: LSystem Char
plantB = LSystem {
    _state = ".",
    _rule = \c -> case c of
        '.' -> "2G[[.]g.]g2m[[2.]2M][m2.]M."
        '2' -> "22"
        _ -> return c
}
-- vim:fdm=marker
