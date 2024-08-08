{-# LANGUAGE DeriveAnyClass #-}

module Player where

import qualified ExtraClasses as EC

import SDLHelper.Data.Rect

import qualified SDL

data Player = Player {
    speed :: Float,
    rect  :: Rect,
    sprite :: (SDL.Texture, SDL.TextureInfo)
}

instance EC.RectContainer Player where
    getRect = rect
    setRect p r = p { rect = r }

instance EC.Entity Player where
    getSprite = sprite