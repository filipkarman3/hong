module SDLHelper.Data.Rect where

import qualified SDL

data Rect = Rect { rectX :: Float, rectY :: Float, rectW :: Float, rectH :: Float } deriving Show

-- centers a rect on another rect
centerRect :: Rect -> Rect -> Rect
centerRect a b = centerRectHorizontal (centerRectVertical a b) b

centerRectVertical :: Rect -> Rect -> Rect
centerRectVertical a b = a { rectY = newY } where
    centerY = rectY b + rectH b /2
    newY    = centerY - rectH a /2

centerRectHorizontal :: Rect -> Rect -> Rect
centerRectHorizontal a b = a { rectX = newX } where
    centerX = rectX b + rectW b /2
    newX    = centerX - rectW a /2

-- converts the rect to a (more convoluted) SDL-friendly format that can be used for rendering
toSDLRect :: a -> a -> a -> a -> SDL.Rectangle a
toSDLRect a b c d = SDL.Rectangle e f where
    e = SDL.P $ SDL.V2 a b
    f = SDL.V2 c d

-- checks if b is fully contained within a
fullyContains :: Rect -> Rect -> Bool
fullyContains a b =
                 rectX a <= rectX b
    &&           rectY a <= rectY b
    && rectX b + rectW b <= rectX a + rectW a
    && rectY b + rectH b <= rectY a + rectH a

-- checks if a and b overlap in any capacity
overlaps :: Rect -> Rect -> Bool
overlaps a b = overlapsVertically a b && overlapsHorizontally a b

overlapsVertically :: Rect -> Rect -> Bool
overlapsVertically a b = f a b || f b a where
    f a b = (rectY a <= rectY b) && (rectY b <= rectY a + rectH a)

overlapsHorizontally :: Rect -> Rect -> Bool
overlapsHorizontally a b = f a b || f b a where
    f a b = (rectX a <= rectX b) && (rectX b <= rectX a + rectW a)