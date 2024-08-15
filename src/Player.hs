module Player where

import qualified SDLHelper.Data.MiscData as MD

import SDLHelper.Data.Rect

import qualified SDL

data Player = Player {
    speed :: Float,
    rect  :: Rect,
    sprite :: MD.Sprite
}

instance MD.Drawable Player where
    getRect = rect
    setRect p r = p { rect = r }
    getSprite = sprite