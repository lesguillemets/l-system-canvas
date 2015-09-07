{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Turtle (Turtle(..), blankTurtle, Command(..), RenderConfig(..)
             , simCommands, drawFit, drawTurtle
             , drawTurtleWithProcess, runCommands) where

import Helpers

import Control.Arrow
import Control.Monad
import Haste.Graphics.Canvas

type Dir = Double

data Turtle = Turtle {
            _loc :: Point,
            _dir :: Dir,
            _memory :: [(Point,Dir)]
}

blankTurtle :: Turtle
blankTurtle = Turtle (0,0) 0 []

forward :: Turtle -> Double -> (Double,Double)
forward t d = ((*d) . cos &&& (*d) . sin) . _dir $ t
(+:) :: (Double,Double) -> (Double,Double) -> (Double,Double)
(a,b) +: (c,d) = (a+c,b+d)

data Command = Jump Double
             | Draw Double
             | Turn Double
             | JumpTo (Double, Double)
             | DrawTo (Double, Double)
             | SetAng Double
             | SetState ((Double, Double), Double)
             | Stay
             | Save
             | Load

-- TODO: Simplify
simCommands :: Turtle -> [Command] -> [[Point]]
simCommands = execCs' [] []
execCs' :: [[Point]] -> [Point] -> Turtle -> [Command] -> [[Point]]
execCs' paths currentPath _ [] = currentPath:paths
execCs' paths currentPath !t@(Turtle{..}) (c:cs) =
    case c of
        (Jump d)     -> case currentPath of
                            [] -> execCs' paths []
                                    t{_loc = _loc +: forward t d} cs
                            cP -> execCs' (cP:paths) []
                                    t{_loc = _loc +: forward t d} cs
        (Draw d)     -> let newLoc = _loc +: forward t d
                            newT = t{_loc = newLoc}
                            in case currentPath of
                                   [] -> execCs' paths [newLoc, _loc] newT cs
                                   cP -> execCs' paths (newLoc:cP) newT cs
        (Turn arg)   -> execCs' paths currentPath t{_dir = _dir + arg} cs
        (JumpTo d)   -> case currentPath of
                            [] -> execCs' paths [] t{_loc = d} cs
                            cP -> execCs' (cP:paths) [] t{_loc = d} cs
        (DrawTo d)   -> case currentPath of
                            [] -> execCs' paths [d, _loc] t{_loc = d} cs
                            cP -> execCs' paths (d:cP) t{_loc = d} cs
        (SetAng dir) -> execCs' paths currentPath t{_dir = dir} cs
        (SetState (loc,dir))
                     -> execCs' paths currentPath t{_dir=dir, _loc=loc} cs
        Stay         -> execCs' paths currentPath t cs
        Save         ->
            execCs' paths currentPath t{_memory = (_loc,_dir):_memory} cs
        Load         -> let
            ((newLoc,newDir):post) = _memory
            newT = t{_loc = newLoc, _dir = newDir, _memory = post}
            in
                case currentPath of
                    [] -> execCs' paths [] newT cs
                    cP -> execCs' (cP:paths) [] newT cs

runCommands :: Turtle -> [Command] -> [Shape ()]
runCommands t = map path . simCommands t

data RenderConfig = RenderConfig {
                  _size :: (Double, Double),
                  _margin :: (Double, Double)
}

-- TODO : line width
drawFit :: Canvas -> RenderConfig -> Turtle -> [Command] -> IO ()
drawFit cnv r t cs = let
    paths = simCommands t cs
    points = concat paths
    (rMost, lMost) =  maxmin . map fst $ points
    (bottom, top) =  maxmin . map snd $ points
    in
        forM_ paths $ renderOnTop cnv . stroke . path . map (
                fit (_size r) (_margin r) (lMost,top) (rMost,bottom)
                )

drawTurtle :: Canvas -> Turtle -> [Command] -> IO ()
drawTurtle = drawTurtleWithProcess id

drawTurtleWithProcess :: (Picture() -> Picture ()) -- configure
                     -> Canvas -- canvas to draw on
                     -> Turtle -- initial turtle
                     -> [Command] -- commands to execute
                     -> IO ()
drawTurtleWithProcess f cnv t =
    mapM_ (renderOnTop cnv . f . stroke) . runCommands t
