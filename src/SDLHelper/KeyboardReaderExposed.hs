{-# LANGUAGE DeriveGeneric #-}

module SDLHelper.KeyboardReaderExposed where

import SDL

import GHC.Generics (Generic)

data Keybind  = Left
              | Right
              | Up
              | Down
              | Quit
              deriving (Eq, Ord, Enum, Bounded, Generic)

getDefaultInputsExposed :: [SDL.Keycode]
getDefaultInputsExposed = [
        SDL.KeycodeLeft,
        SDL.KeycodeRight,
        SDL.KeycodeUp,
        SDL.KeycodeDown,
        SDL.KeycodeEscape
     ]