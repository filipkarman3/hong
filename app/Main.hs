{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Player as P

import qualified SDL

import qualified SDLHelper.SDLHelper as H

import           SDLHelper.Data.Rect
import qualified SDLHelper.Data.Keyboard              as KB (Keyboard)
import qualified SDLHelper.Data.KeyboardReaderExposed as KB (Keybind(..))
import qualified SDLHelper.Data.WorldExposed          as W

import qualified SDLHelper.KeyboardReader as KB
import SDLHelper.Monads

import Control.Monad.IO.Class (MonadIO, liftIO)

import Foreign.C.Types (CInt)
import Data.Function ((&))

screenWidth = 1000
screenHeight = 600

data Direction = Left | Right | Up | Down deriving Show

main :: IO ()
main = H.doMain "hsgaem" (screenWidth, screenHeight) "assets/data/layout.kb" Main.init loop terminate

init :: W.WorldRaw -> IO W.World
init raw = do
    playerImg <- H.loadTexture (W.r raw) "assets/player.png"

    let player = P.Player {
        P.speed = 3,
        P.rect  = let
            screenRect = Rect 0 0 screenWidth screenHeight
            playerRect = Rect 0 0 (fromIntegral $ SDL.textureWidth $ snd playerImg) (fromIntegral $ SDL.textureHeight $ snd playerImg)
            in H.centerRect playerRect screenRect,
        P.sprite = playerImg
    }

    let world = W.World {
        W.wr     = raw,
        W.player = player
    }

    pure world

loop :: W.World -> IO W.World
loop w = do
    w' <- registerQuit w >>= movePlayer

    let p' = W.player w'
    renderSimple w (P.sprite p') (P.playerPos p')

    pure w'

-- checks if the player is trying to exit the game, and flips the quit flag if so
registerQuit :: W.World -> IO W.World
registerQuit w = ifM (KB.isKeyDown w KB.Quit) (W.setQuit w True) w

renderSimple :: W.World -> (SDL.Texture, SDL.TextureInfo) -> (SDL.V2 Int) -> IO ()
renderSimple w (t, i) (SDL.V2 x y) = SDL.copy (W.getR w) t Nothing (Just rect) where
    rect   = H.toSDLRect (toCInt x) (toCInt y) width height
    width  = SDL.textureWidth i
    height = SDL.textureHeight i

    toCInt :: Int -> CInt
    toCInt = fromIntegral

-- handles player movement
movePlayer :: W.World -> IO W.World
movePlayer w = do
    w' <- controlPlayer w     -- first, you can move the player however you like using the input controller (keyboard, controller, whatever)
    pure $ constrainPlayer w' -- but we can't have the player going off the screen! Then it's time to constrain the player's movement to acceptable bounds

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
    x = rectX r
    y = rectY r
    
    -- keep the player on screen
    r' = r {
        rectX = constrain 0 (screenWidth - rectW r) (rectX r),
        rectY = constrain 0 (screenHeight - rectH r) (rectY r)
    }
    p' = p { P.rect = r'}

-- prevent the player from going offscreen
constrain :: (Ord a) => a -> a -> a -> a
constrain min max x = if x < min then min
    else if x > max then max
    else x

terminate :: (MonadIO m) => W.World -> m ()
terminate w = do
    SDL.destroyTexture (fst $ P.sprite $ W.player w)