module LSystem where

data LSystem a = LSystem {
    _state :: [a],
    _rule :: a -> [a]
}

nextGen :: LSystem a -> LSystem a
nextGen s = s {_state = concatMap (_rule s) (_state s)}
