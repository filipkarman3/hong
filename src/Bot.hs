module Bot where

import qualified SDL

import SDLHelper.Data.Rect (Rect)
import qualified SDLHelper.Data.MiscData as MD

data Bot = Bot {
    speed :: Float,
    rect  :: Rect,
    sprite :: MD.Sprite,
    destination :: Int
}

instance MD.Drawable Bot where
    getRect = rect
    setRect b r = b { rect = r }
    getSprite = sprite