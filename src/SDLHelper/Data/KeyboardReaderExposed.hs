{-# LANGUAGE DeriveGeneric #-}

module SDLHelper.Data.KeyboardReaderExposed where

import SDL

import GHC.Generics (Generic)

data Keybind  = Left
              | Right
              | Up
              | Down
              | Quit
              deriving (Eq, Ord, Enum, Bounded, Generic)

getDefaultInputsExposed :: [SDL.Scancode]
getDefaultInputsExposed = [
        SDL.ScancodeLeft,
        SDL.ScancodeRight,
        SDL.ScancodeUp,
        SDL.ScancodeDown,
        SDL.ScancodeEscape
     ]