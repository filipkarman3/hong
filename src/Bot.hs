module Bot where

import qualified SDL

import SDLHelper.Data.Rect (Rect)
import qualified ExtraClasses as EC

data Bot = Bot {
    speed :: Float,
    rect  :: Rect,
    sprite :: (SDL.Texture, SDL.TextureInfo),
    destination :: Int
}

instance EC.RectContainer Bot where
    getRect = rect
    setRect b r = b { rect = r }

instance EC.Entity Bot where
    getSprite = sprite