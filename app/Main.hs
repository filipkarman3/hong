{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import qualified SDLHelper as H
import qualified KeyboardReader as KB

import Data.Word (Word8)

import Control.Monad.IO.Class (MonadIO)


data World = World {
    kb :: KB.Keyboard,
    w  :: SDL.Window,
    sc :: SDL.Surface,
    r  :: SDL.Renderer,
    fps :: Int
}

instance H.World World where
    kb = kb
    w  = w
    sc = sc
    r  = r
    fps = fps

main :: IO ()
main = H.withSDL
    $ H.withWindow "hsgaem" (1000, 600)
    $ \w -> H.withRenderer w
    $ \r -> KB.withKeyboard "assets/data/layout.kb"
    $ \kb -> do
        screen <- SDL.getWindowSurface w -- gets the surface (place where you actually draw stuff) from a window record

        -- game loop
        -- world' <- H.loop loop $ initialWorld kb w screen
        world' <- H.loop loop $ World kb w screen r 50

        -- SDL.freeSurface img
        SDL.freeSurface $ screen

        -- gotta return the keyboard layout
        pure $ getUpdatedKb world'
    
    where
        -- initialWorld kb w sc = World (kb :: KB.Keyboard) (w :: SDL.Window) (sc :: Ptr SDL.Surface)
        
        getUpdatedKb :: World -> KB.Keyboard
        getUpdatedKb world = kb world

loop :: (MonadIO m) => World -> [SDL.EventPayload] -> m World
loop world es = do
    