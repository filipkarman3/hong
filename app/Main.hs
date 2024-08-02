{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Player as P

import qualified SDL

import SDLHelper.WorldExposed as W

import qualified SDLHelper.SDLHelper as H
import SDLHelper.Data
import qualified SDLHelper.KeyboardReader as KB
import qualified SDLHelper.KeyboardReaderExposed as KB (Keybind(..))

import Control.Monad.IO.Class (MonadIO, liftIO)

import Foreign.C.Types (CInt)
import Data.Function ((&))

screenWidth = 1000
screenHeight = 600

data Direction = Left | Right | Up | Down deriving Show

main :: IO ()
main = H.doMain "hsgaem" (screenWidth, screenHeight) "assets/data/layout.kb" Main.init loop terminate

init :: KB.Keyboard -> SDL.Window -> SDL.Renderer -> IO World
init kb w r = do
    playerImg <- H.loadTexture r "assets/player.png"

    let player = P.Player {
        P.speed = 3,
        P.rect  = let
            screenRect = Rect 0 0 screenWidth screenHeight
            playerRect = Rect 0 0 (fromIntegral $ SDL.textureWidth $ snd playerImg) (fromIntegral $ SDL.textureWidth $ snd playerImg)
            in H.centerRect playerRect screenRect,
        P.sprite = playerImg
    }

    let world = World {
        kb = kb,
        w  = w,
        r  = r,
        es = [],
        fps = 50,
        player = player,
        quit = False
    }

    pure world

loop :: (MonadIO m) => World -> m World
loop w = do
    let w' = registerQuit w & controlPlayer

    let p' = W.player w'
    renderSimple w (P.sprite p') (P.playerPos p')

    pure w'

-- checks if the player is trying to exit the game, and flips the quit flag if so
registerQuit :: W.World -> W.World
registerQuit w = if KB.isKeyDown (W.kb w) KB.Quit (W.es w) then
        w { W.quit = True }
    else w

renderSimple :: (MonadIO m) => World -> (SDL.Texture, SDL.TextureInfo) -> (SDL.V2 Int) -> m ()
renderSimple w (t, i) (SDL.V2 x y) = SDL.copy (r w) t Nothing (Just rect) where
    rect = H.toSDLRect (toCInt x) (toCInt y) width height
    width  = SDL.textureWidth i
    height = SDL.textureHeight i

    toCInt :: Int -> CInt
    toCInt = fromIntegral

-- handles player movement
movePlayer :: W.World -> W.World
movePlayer w =
    constrainPlayer $ controlPlayer w -- first, you can move the player however you like using the input controller (keyboard, controller, whatever)
     -- but we can't have the player going off the screen! Then it's time to constrain the player's movement to acceptable bounds

-- handles player movement using the input controller
controlPlayer :: W.World -> W.World
controlPlayer w =
    -- update the world record with the player after they've moved
    w { player = p' } where

    -- to move the player, I fold over a list of: (
    --     the keybind corresponding to the direction you're heading in,
    --     the function that will be used to change the position of the player,
    --     the sign of the direction to move in
    -- )

    -- IE: (Left, P.changePlayerX, (-)) means:
    --     if the player hits the left button, the player will move along the X axis in the negative direction

    -- when folding over each element, the function f is called, more on that below
    p' = foldr f (W.player w) [
            (KB.Left,  P.changePlayerX, (-)),
            (KB.Right, P.changePlayerX, (+)),
            (KB.Up,    P.changePlayerY, (-)),
            (KB.Down,  P.changePlayerY, (+))
        ]

    -- for each element:
    -- 1) check if the corresponding key is pressed down
    -- 2) if so, update the player's position and return the updated player
    --    otherwise, return the unchanged player
    f (d, m, s) p = if KB.isKeyDown (W.kb w) d (W.es w) then
            m p (s 0 (P.speed p))
        else p

-- prevents the player from going offscreen
constrainPlayer :: W.World -> W.World
constrainPlayer w =
    -- return a world record with the updated player
    w { player = p' } where
    
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

terminate :: (MonadIO m) => World -> m ()
terminate w = do
    SDL.destroyTexture (fst $ P.sprite $ player w)