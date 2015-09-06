{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Turtle (Turtle(..), blankTurtle, Command(..)
             , drawTurtle, drawTurtleWithConfig, runCommands) where

import Control.Arrow
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

-- TODO : prefer [[Point]] ?
runCommand :: Turtle -> Command -> (Turtle, Maybe (Shape ()))
runCommand !t@(Turtle{..}) c =
    case c of
        (Jump d)     -> (t{_loc = _loc +: forward t d}, Nothing)
        (Draw d)     -> let newLoc = _loc +: forward t d in
            (t{_loc = newLoc}, Just $ path [_loc, newLoc])
        (Turn arg)   -> (t{_dir = _dir + arg}, Nothing)
        (JumpTo d)   -> (t{_loc = d}, Nothing)
        (DrawTo d)   -> (t{_loc = d}, Just $ path [_loc, d])
        (SetAng dir) -> (t{_dir = dir}, Nothing)
        (SetState (loc,dir))
                     -> (t{_dir=dir, _loc=loc}, Nothing)
        Stay         -> (t, Nothing)
        Save         -> (t{_memory = (_loc,_dir):_memory}, Nothing)
        Load         -> let ((newLoc,newDir):post) = _memory in
            (t{_loc = newLoc, _dir = newDir, _memory = post}, Nothing)

runCommands :: Turtle -> [Command] -> [Shape ()]
runCommands _ [] = []
runCommands t (c:cs) = let (nt, shape) = runCommand t c in
    case shape of
        Nothing -> runCommands nt cs
        (Just s) -> s:runCommands nt cs

drawTurtle :: Canvas -> Turtle -> [Command] -> IO ()
drawTurtle = drawTurtleWithConfig id

drawTurtleWithConfig :: (Picture() -> Picture ()) -- configure
                     -> Canvas -- canvas to draw on
                     -> Turtle -- initial turtle
                     -> [Command] -- commands to execute
                     -> IO ()
drawTurtleWithConfig f cnv t =
    mapM_ (renderOnTop cnv . f . stroke) . runCommands t
