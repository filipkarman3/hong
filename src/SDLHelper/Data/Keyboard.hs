{-# LANGUAGE TypeSynonymInstances #-}

module SDLHelper.Data.Keyboard where

import SDL

import qualified Data.Map as Map

import qualified SDLHelper.Data.KeyboardReaderExposed as KR (Keybind)
import Data.Aeson as Aeson

type Keyboard = Map.Map KR.Keybind KeycodeWrapper

-- needed for encoding of Keybinds to work
instance Aeson.ToJSON KR.Keybind where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON KR.Keybind

-- these two are needed because map keys are encoded differently I think?
instance Aeson.ToJSONKey KR.Keybind

instance Aeson.FromJSONKey KR.Keybind


-- this type synonym has been defined because I don't want to directly interact with and make extensions to the SDL.Keycode class
type KeycodeWrapper = SDL.Scancode

-- needed for encoding and decoding of SDL keypresses to work
instance Aeson.ToJSON KeycodeWrapper where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON KeycodeWrapper