module ExtraClasses where

import qualified SDL

import SDLHelper.Data.Rect (Rect, rectX, rectY)

class RectContainer a where
    getRect :: a -> Rect
    setRect :: a -> Rect -> a

    getPos :: a -> SDL.V2 Int
    getPos a = SDL.V2 x y where
        x = toInt $ rectX r
        y = toInt $ rectY r
        r = getRect a
    
    changeX :: a -> Float -> a
    changeX a d = setRect a r' where
        r  = getRect a
        r' = r { rectX = rectX r + d }

    changeY :: a -> Float -> a
    changeY a d = setRect a r' where
        r  = getRect a
        r' = r { rectY = rectY r + d }

class (RectContainer a) => Entity a where
    getSprite :: a -> (SDL.Texture, SDL.TextureInfo)

toInt :: Float -> Int
toInt = round