module LSystem where
import Data.List (foldl')
import Data.Function ((&))

-- TODO : perhaps we should save accompanying translator
data LSystem a = LSystem {
    _state :: [a],
    _rule :: a -> [a]
}

nextGen :: LSystem a -> LSystem a
nextGen s = s {_state = concatMap (_rule s) (_state s)}

nthGen :: LSystem a -> Int -> LSystem a
nthGen s n = foldl' (&) s $ replicate n nextGen
