{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (Left, Right)

import qualified Player       as P
import qualified Ball         as B
import qualified Bot          as Bot
import qualified ExtraClasses as EC

import qualified SDL

import qualified SDLHelper.SDLHelper as H

import qualified SDLHelper.Data.Rect                  as R
import qualified SDLHelper.Data.Keyboard              as KB (Keyboard)
import qualified SDLHelper.Data.KeyboardReaderExposed as KB (Keybind(..))
import qualified SDLHelper.Data.WorldExposed          as W

import qualified SDLHelper.KeyboardReader as KB
import SDLHelper.Monads

import Control.Monad.IO.Class (MonadIO, liftIO)

import Foreign.C.Types (CInt)
import Data.Function ((&))
import GHC.Float (int2Float)

import System.Random.Stateful (uniformRM, globalStdGen, randomRIO)

-------------- HELPFUL DATATYPES AND FUNCTIONS --------------
data Direction = Left | Right | Up | Down deriving (Show, Eq)

screenWidth :: Int
screenWidth = 1000

screenHeight :: Int
screenHeight = 600

-- can be used for centering sprites on the screen
screenRect = R.Rect 0 0 (int2Float screenWidth) (int2Float screenHeight)

-- create a rect for a sprite at (0, 0)
getOriginRect :: (SDL.Texture, SDL.TextureInfo) -> R.Rect
getOriginRect t = R.Rect 0 0 w h where
    w = fromIntegral $ SDL.textureWidth  $ snd t
    h = fromIntegral $ SDL.textureHeight $ snd t

toInt :: Float -> Int
toInt = round

-------------- CALL TO SDLHELPER --------------
main :: IO ()
main = H.doMain "hsgaem" (screenWidth, screenHeight) "assets/data/layout.kb" Main.init loop terminate

-------------- INITIALISATION LOGIC --------------
init :: W.WorldRaw -> IO W.World
init raw = do
    -- required for player generation
    playerImg <- H.loadTexture (W.r raw) "assets/player.png"

    -- required for ball generation
    randNum <- randomRIO (0, 3) :: IO Int
    ballImg <- H.loadTexture (W.r raw) "assets/ball.png"

    let p = initPlayer playerImg

    let world = W.World {
        W.wr     = raw,
        W.player = p,
        W.ball   = initBall ballImg randNum (W.fps raw),
        W.score  = (0, 0),
        W.bot    = initBot p
    }

    -- calculates where the bot will go to intercept the incoming ball
    pure $ calculateNewBotDestination world

initPlayer :: (SDL.Texture, SDL.TextureInfo) -> P.Player
initPlayer t = P.Player {
        P.speed = 3,
        P.rect  = R.centerRectVertical (getOriginRect t) screenRect,
        P.sprite = t
    }

initBall :: (SDL.Texture, SDL.TextureInfo) -> Int -> Int -> B.Ball
initBall t rn fps = B.Ball {
        B.speed = 3,
        B.rect  = R.centerRect (getOriginRect t) screenRect,
        B.sprite = t,
        B.speedIncrement = 1,
        B.timeUntilIncrementMax = incT,
        B.timeUntilIncrement = incT,
        B.angle = rn * 2 + 1
    } where
        incT = 5 * fps

initBot :: P.Player -> Bot.Bot
initBot p = Bot.Bot {
        Bot.speed = P.speed p,
        Bot.rect = r',
        Bot.sprite = P.sprite p
    } where
        r  = P.rect p
        r' = r { R.rectX = int2Float screenWidth - R.rectW r }

-------------- GAME LOOP --------------
loop :: W.World -> IO W.World
loop w = do
    -- update the world
    w' <- updateWorld w

    -- perform rendering
    renderWorld w'

    pure w'

-- update the world
updateWorld :: W.World -> IO W.World
updateWorld w = do
    w' <- registerQuit w >>= movePlayer >>= updateBall >>= updateBot
    pure w'

-- checks if the player is trying to exit the game, and flips the quit flag if so
registerQuit :: W.World -> IO W.World
registerQuit w = ifM (KB.isKeyDown w KB.Quit) (W.setQuit w True) w

-------------- RENDERING --------------
renderWorld :: W.World -> IO ()
renderWorld w = do
    renderEntity w (W.player w)
    renderEntity w (W.ball w)
    renderEntity w (W.bot w)

-- render stuff onto the screen
renderSimple :: W.World
             -> (SDL.Texture, SDL.TextureInfo)
             -> (SDL.V2 Int)
             -> IO ()
renderSimple w (t, i) (SDL.V2 x y) = SDL.copy (W.getR w) t Nothing (Just rect) where
    rect   = R.toSDLRect (toCInt x) (toCInt y) width height
    width  = SDL.textureWidth i
    height = SDL.textureHeight i

    toCInt :: Int -> CInt
    toCInt = fromIntegral

-- abstracts renderSimple for instances of Entity
renderEntity :: (EC.Entity e)
             => W.World
             -> e
             -> IO ()
renderEntity w e = renderSimple w (EC.getSprite e) (EC.getPos e)

-------------- PLAYER MOVEMENT --------------
-- handles player movement
movePlayer :: W.World -> IO W.World
movePlayer w = do
    -- first, you can move the player however you like using the input controller (keyboard, controller, whatever)
    w' <- controlPlayer w
    -- but we can't have the player going off the screen! Then it's time to constrain the player's movement to acceptable bounds
    pure $ constrainPlayer w'

-- handles player movement using the input controller
controlPlayer :: W.World -> IO W.World
controlPlayer w = do
    w'  <- registerPress w  KB.Up (-n)
    w'' <- registerPress w' KB.Down n
    pure w'' where
       n = P.speed $ W.player w
       registerPress w k n = ifM (KB.isKeyDown w k) (W.changePlayerY w n) w


-- prevents the player from going offscreen
constrainPlayer :: W.World -> W.World
constrainPlayer w =
    -- return a world record with the updated player
    w { W.player = p' } where

    -- yaay I love unpacking values
    p = W.player w
    r = P.rect p
    x = R.rectX r
    y = R.rectY r

    -- keep the player on screen
    r' = r {
        R.rectX = constrain 0 ((int2Float screenWidth)  - R.rectW r) (R.rectX r),
        R.rectY = constrain 0 ((int2Float screenHeight) - R.rectH r) (R.rectY r)
    }
    p' = p { P.rect = r'}

-- prevent the player from going offscreen
constrain :: (Ord a) => a -> a -> a -> a
constrain min max x 
    | x < min = min
    | x > max = max
    | otherwise = x

-------------- BALL COMPUTATION --------------
updateBall :: W.World -> IO W.World
updateBall w = pure $ moveBall w & handleBallCollision & increaseBallSpeed

moveBall :: W.World -> W.World
moveBall w = w { W.ball = b' } where
    b  = W.ball w
    a  = (int2Float $ B.angle b) * pi/4
    b' = EC.changeY (EC.changeX b xSpeed) ySpeed
    xSpeed = sin a * (-(B.speed b))
    ySpeed = cos a * B.speed b

-- reflect the ball if it hits the side of the screen or paddles
handleBallCollision :: W.World -> W.World
-- check the definition of a'' for an explanation of this if statement
handleBallCollision w = if a' == a'' then w' else calculateNewBotDestination w' where
    -- get some values that will be helpful
    b  = W.ball w
    r  = B.rect b
    a  = B.angle b

    -- if the ball hits the paddles or sides of the screen, it bounces
    -- a "bounce" is a change of angle that the ball is travelling at
    -- this function funnels the angle the ball is travelling at through three different functions
    -- each function handles a reflection off a different side of the screen
    a' = adjustAngle [reflectOffCeiling, reflectOffFloor, reflectOffPlayer] r a

    -- the last reflection (against the bot's paddle) is done separately
    -- this way, we can compare the angle right before the function is ran and right afterwards
    -- if the pre-function and post-function angles are different, a reflection has occured
    -- if this occurs, the bot has hit the ball and now needs to calculate the next position to move to
    a'' = reflectOffBot r a'

    -- this saves the new angle before putting the updated ball back in the world record
    b' = B.setAngle b a''
    w' = w { W.ball = b' }

    -- four functions causing a reflection to happen if a condition is met
    reflectOffPlayer  r a = genericReflection ((P.rect $ W.player w) `R.overlaps` r)            B.angleReflectionLeft   a
    reflectOffCeiling r a = genericReflection ((R.rectY r) < 0)                                 B.angleReflectionTop    a
    reflectOffFloor   r a = genericReflection (int2Float screenHeight <= R.rectY r + R.rectH r) B.angleReflectionBottom a
    reflectOffBot     r a = genericReflection ((Bot.rect $ W.bot w) `R.overlaps` r)             B.angleReflectionRight  a

    genericReflection :: Bool -> (Int -> Int) -> Int -> Int
    genericReflection cond f a
        | cond      = f a
        | otherwise = a

    adjustAngle :: [R.Rect -> Int -> Int] -> R.Rect -> Int -> Int
    adjustAngle fs r a = applyAll (map (\f -> f r) fs) a

    applyAll :: [a -> a] -> a -> a
    applyAll fs x = foldr (\f x' -> f x') x fs

increaseBallSpeed :: W.World -> W.World
increaseBallSpeed w = w { W.ball = b' } where
    b' 
        | B.timeUntilIncrement b == 0 = b {
                B.speed = B.speed b + B.speedIncrement b,
                B.timeUntilIncrement = B.timeUntilIncrementMax b
            }
        | otherwise                   = b {
                B.timeUntilIncrement = B.timeUntilIncrement b - 1
            }
    b = W.ball w


-------------- BOT HANDLING --------------
updateBot :: W.World -> IO W.World
updateBot w = pure $ w { W.bot = b' } where
    b = W.bot w
    b'
        -- | botReachedDestination b = EC.changeY b 3
        -- | otherwise               = EC.changeY b 3
        | botReachedDestination b = b
        | otherwise               = botMoveToDestination b

botReachedDestination :: Bot.Bot -> Bool
botReachedDestination b = within currentPos minY maxY where
    currentPos = toInt $ R.rectY $ Bot.rect b
    minY       = d - s
    maxY       = d + s
    d          = Bot.destination b
    s          = toInt $ Bot.speed b

within :: (Ord a) => a -> a -> a -> Bool
within x min max = min <= x && x <= max

botMoveToDestination :: Bot.Bot -> Bot.Bot
botMoveToDestination b
    | d < R.rectY r = EC.changeY b (-s)
    | otherwise     = EC.changeY b s
    where
        r = Bot.rect b
        s = Bot.speed b
        d = int2Float $ Bot.destination b

data XorY = X | Y

calculateNewBotDestination :: W.World -> W.World
calculateNewBotDestination w = w { W.bot = b' } where
    -- update the ball with the new destination
    b' = b { Bot.destination = d' }
    b  = W.bot w

    -- calculate the new destination
    -- (ie: where ball will hit the right side of the screen next)
    d' = nextColLoc (ballX, ballY) (B.angle ball)
    ballX = toInt $ R.rectX ballR
    ballY = toInt $ R.rectY ballR

    -- some more data we'll need to use
    ballR = B.rect ball
    ball  = W.ball w

    -- this function runs recursively, continuously calculating the next ball collision
    -- the function stops once it's found out where it will hit the right side of the screen
    nextColLoc :: (Int, Int) -> Int -> Int
    nextColLoc (x, y) a
        -- if the next predicted hit is the right side of the screen, stop the recursion
        -- returns the height at which the ball will hit the right side of the screen
        | predictedRightHit = optimalPos

        -- recursively calculate ball hits until the ball hits the right side of the screen
        | otherwise         = nextColLoc (colLocX, colLocY) a' where

            -- hardcoded. Gets the diagonal direction of the ball as a tuple
            -- eg: (Down, Left)
            (xDir, yDir) = getDir a

            -- defines what I actually mean by left/right side of the screen
            -- I've been saying "right side of the screen" a lot
            -- but I actually mean the left side of the bot's paddle
            -- which is what the ball will (hopefully) collide with
            leftBound  = toInt $ R.rectW $ P.rect $ W.player w
            rightBound = screenWidth - toInt (R.rectW $ Bot.rect b) - toInt (R.rectW $ B.rect $ W.ball w)
            upBound    = 0
            downBound  = screenHeight - toInt (R.rectH $ B.rect $ W.ball w)
            
            -- assuming the ball can't collide with the top and bottom:
            -- xColDist is what the ball will first collide with
            -- (either the left or right side of the screen)
            xColDist
                | xDir == Left = x - leftBound  -- if the ball's going left, it's gonna hit the left side of the screen
                | otherwise    = rightBound - x
            
            -- same as xColDist but for y
            yColDist
                | yDir == Up   = y - upBound    
                | otherwise    = downBound - y
            
            -- get the first collision that will happen
            -- obviously, if the bottom of the screen is closer than the left side
            -- of the screen, you're gonna hit the bottom first
            closerCol = min xColDist yColDist

            -- so at the time of collision, these should be the
            -- x and y coords of the ball
            colLocX   = if xDir == Left then x-closerCol else x+closerCol
            colLocY   = if yDir == Up   then y-closerCol else y+closerCol

            -- these values tell you which side has been hit
            predictedLeftHit   = (xDir == Left)  && (xColDist == closerCol)
            predictedRightHit  = (xDir == Right) && (xColDist == closerCol)
            predictedTopHit    = (yDir == Up)    && (yColDist == closerCol)
            predictedBottomHit = (yDir == Down)  && (yColDist == closerCol)

            -- this checks if a side has been hit and adjusts the angle accordingly
            -- folds over a zipped list in the form [(f1a, f1b), (f2a, f2b), ...]
            -- f_a functions tell you if a wall has been collided with
            -- if an f_a function returns True, the corresponding f_b function is called
            -- f_b functions modify the angle of the ball
            a' = foldr (\(f1, f2) da -> if f1 then f2 da else da) a l
            l = zip
                [predictedLeftHit, predictedRightHit, predictedTopHit, predictedBottomHit]
                [B.angleReflectionLeft, B.angleReflectionRight, B.angleReflectionTop, B.angleReflectionBottom]

            -- adjusts the destination that the bot goes to
            -- bot will attempt to move to a position that is
            -- (position where ball will hit) - (height of bot / 2)
            optimalPos = colLocY - toInt ((R.rectH $ Bot.rect b)/2)

    p = W.player w
    
    getDir :: Int -> (Direction, Direction)
    getDir 1 = (Left,  Down)
    getDir 3 = (Left,  Up)
    getDir 5 = (Right, Up)
    getDir _ = (Right, Down)

-------------- TERMINATION --------------
terminate :: (MonadIO m) => W.World -> m ()
terminate w = do
    SDL.destroyTexture (fst $ P.sprite $ W.player w)
    SDL.destroyTexture (fst $ B.sprite $ W.ball   w)
