module SDLHelper.Data.Rect (centerRect, Rect(..), centerRectVertical, centerRectHorizontal, toSDLRect) where

import qualified SDL

data Rect = Rect { rectX :: Int, rectY :: Int, rectW :: Int, rectH :: Int }

-- centers a rect on another rect
centerRect :: Rect -> Rect -> Rect
centerRect a b = a { rectX = newX, rectY = newY } where
    centerX = (toRational . rectX) b + (toRational . rectW) b /2
    centerY = (toRational . rectY) b + (toRational . rectH) b /2
    newX    = toInt $ centerX - (toRational . rectW) a /2
    newY    = toInt $ centerY - (toRational . rectH) a /2


centerRectVertical :: Rect -> Rect -> Rect
centerRectVertical a b = a { rectY = newY } where
    centerY = (toRational . rectY) b + (toRational . rectH) b /2
    newY    = toInt $ centerY - (toRational . rectH) a /2

centerRectHorizontal :: Rect -> Rect -> Rect
centerRectHorizontal a b = a { rectX = newX } where
    centerX = (toRational . rectX) b + (toRational . rectW) b /2
    newX    = toInt $ centerX - (toRational . rectW) a /2

toInt :: (RealFrac a) => a -> Int
toInt = round

-- converts the rect to a (more convoluted) SDL-friendly format that can be used for rendering
toSDLRect :: a -> a -> a -> a -> SDL.Rectangle a
toSDLRect a b c d = SDL.Rectangle e f where
    e = SDL.P $ SDL.V2 a b
    f = SDL.V2 c d