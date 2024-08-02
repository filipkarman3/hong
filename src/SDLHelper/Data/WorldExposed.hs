module SDLHelper.Data.WorldExposed where

import qualified SDL

import qualified SDLHelper.Data.Keyboard as KB

import qualified Player as P

data World = World {
    kb :: KB.Keyboard,
    kbs :: SDL.Scancode -> Bool,
    kbps :: SDL.Scancode -> Bool,
    w  :: SDL.Window,
    es :: [SDL.EventPayload],
    r  :: SDL.Renderer,
    fps :: Int,
    quit :: Bool,
    player :: P.Player
}

changePlayerX :: World -> Int -> World
changePlayerX w n = w { player = p' } where
    p = player w
    p' = P.changePlayerX p n

changePlayerY :: World -> Int -> World
changePlayerY w n = w { player = p' } where
    p = player w
    p' = P.changePlayerY p n