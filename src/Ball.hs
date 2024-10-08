{-# LANGUAGE DeriveAnyClass #-}

module Ball where

import qualified SDL

import SDLHelper.Data.Rect (Rect)
import qualified SDLHelper.Data.MiscData as MD

data Ball = Ball {
    initialSpeed :: Float,
    speed :: Float,
    rect  :: Rect,
    sprite :: MD.Sprite,
    speedIncrement :: Float,
    timeUntilIncrementMax :: Int,
    timeUntilIncrement :: Int,
    angle :: Int
}

setAngle :: Ball -> Int -> Ball
setAngle b a = b { angle = correctAngle a } where
    correctAngle x
        | x < 0     = correctAngle $ x + 8
        | x > 8     = correctAngle $ x - 8
        | otherwise = x

angleReflectionBottom :: Int -> Int
angleReflectionBottom 7 = 5
angleReflectionBottom 1 = 3
angleReflectionBottom x = x

angleReflectionTop :: Int -> Int
angleReflectionTop 3 = 1
angleReflectionTop 5 = 7
angleReflectionTop x = x

angleReflectionLeft :: Int -> Int
angleReflectionLeft 1 = 7
angleReflectionLeft 3 = 5
angleReflectionLeft x = x

angleReflectionRight :: Int -> Int
angleReflectionRight 5 = 3
angleReflectionRight 7 = 1
angleReflectionRight x = x

instance MD.Drawable Ball where
    getRect = rect
    setRect b r = b { rect = r }
    getSprite = sprite