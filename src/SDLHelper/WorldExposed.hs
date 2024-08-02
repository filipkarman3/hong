module SDLHelper.WorldExposed where

import qualified SDL

import qualified SDLHelper.KeyboardReader as KB

import Player (Player)

data World = World {
    kb :: KB.Keyboard,
    w  :: SDL.Window,
    es :: [SDL.EventPayload],
    r  :: SDL.Renderer,
    fps :: Int,
    quit :: Bool,
    player :: Player
}