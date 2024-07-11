module SDLHelper.WorldExposed where

import qualified SDL

import qualified SDLHelper.KeyboardReader as KB

data World = World {
    kb :: KB.Keyboard,
    w  :: SDL.Window,
    sc :: SDL.Surface,
    r  :: SDL.Renderer,
    fps :: Int
}