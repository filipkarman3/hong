module Player where

import SDLHelper.Data.Rect

import qualified SDL

data Player = Player {
    speed :: Int,
    rect  :: Rect,
    sprite :: (SDL.Texture, SDL.TextureInfo)
}

changePlayerX :: Player -> Int -> Player
changePlayerX p d = p { rect = r' } where
    r  = rect p
    r' = r { rectX = rectX r + d }

changePlayerY :: Player -> Int -> Player
changePlayerY p d = p { rect = r' } where
    r  = rect p
    r' = r { rectY = rectY r + d }

playerPos :: Player -> SDL.V2 Int
playerPos p = SDL.V2 x y where
    x = rectX r
    y = rectY r
    r = rect p