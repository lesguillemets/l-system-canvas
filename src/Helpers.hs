module Helpers (
               maxmin
             , fit
               ) where
import Data.List
import Haste.Graphics.Canvas

maxmin :: Ord a => [a] -> (a, a)
maxmin xs = foldl' f (head xs, head xs) (tail xs)
    where
        f (ma,mi) elm
            | elm > ma = (elm, mi)
            | mi > elm = (ma, elm)
            | otherwise = (ma, mi)

fit :: (Double, Double) -- the size
    -> (Double, Double) -> (Double, Double) -- topLeft, bottomRight
    -> Point -> Point
fit (w,h) (lft,top) (rgt,btm) (x,y) = let rat = min (w/(rgt-lft)) (h/(btm-top))
        in
            (rat * (x-lft), rat * (y-top))
