{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Player as P

import qualified SDL

import SDLHelper.WorldExposed

import qualified SDLHelper.SDLHelper as H
import SDLHelper.Data
import qualified SDLHelper.KeyboardReader as KB

import Control.Monad.IO.Class (MonadIO)

import Foreign.C.Types (CInt)

screenWidth = 1000
screenHeight = 600

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
        fps = 50,
        player = player
    }

    pure world

loop :: (MonadIO m) => World -> [SDL.EventPayload] -> m World
loop w es = do
    let w' = w { player = P.changePlayerX (player w) (P.speed $ player w) }
    renderSimple w (P.sprite $ player w) (P.playerPos $ player w)
    pure w'

renderSimple :: (MonadIO m) => World -> (SDL.Texture, SDL.TextureInfo) -> (SDL.V2 Int) -> m ()
renderSimple w (t, i) (SDL.V2 x y) = SDL.copy (r w) t Nothing (Just rect) where
    rect = H.toSDLRect (toCInt x) (toCInt y) width height
    width  = SDL.textureWidth i
    height = SDL.textureHeight i

    toCInt :: Int -> CInt
    toCInt = fromIntegral

terminate :: (MonadIO m) => World -> m ()
terminate w = do
    SDL.destroyTexture (fst $ P.sprite $ player w)