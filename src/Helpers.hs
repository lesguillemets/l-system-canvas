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
    -> (Double, Double) -- margin
    -> (Double, Double) -> (Double, Double) -- topLeft, bottomRight
    -> Point -> Point
fit (w,h) (marginTop, marginSide) (lft,top) (rgt,btm) (x,y) =
    let rat = min ((w-2*marginSide)/(rgt-lft)) ((h-2*marginTop)/(btm-top))
        in
            (marginSide + rat * (x-lft), marginTop + rat * (y-top))
