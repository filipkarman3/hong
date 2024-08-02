module SDLHelper.KeyboardReader where

import qualified SDL

import qualified SDLHelper.Data.KeyboardReaderExposed as KR (getDefaultInputsExposed, Keybind(..))
import qualified SDLHelper.Data.WorldExposed          as W  (World, getKb, getKbs, getKbps)
import           SDLHelper.Data.Keyboard                    (Keyboard)

import Control.Monad.IO.Class (MonadIO)

import System.Directory (doesFileExist)

import qualified Data.ByteString      as BS  (readFile, writeFile, ByteString, toStrict)
import qualified Data.Map.Strict      as Map
import qualified Data.Aeson           as Aeson

import Data.Maybe (fromJust, isNothing)


-- loads a keyboard layout given a path to a file storing the keyboard layout
-- then runs some operation
withKeyboard :: String -> (Keyboard -> IO Keyboard) -> IO ()
withKeyboard path op = do
    -- load a keyboard layout, or use the default one in case of an error
    keyboard <- getKeyboard

    -- running the operation
    -- returning the updated keyboard layout
    updatedKeyboard <- op keyboard

    -- saving the updated keyboard layout
    BS.writeFile path $ serialiseLayout updatedKeyboard
    
    where
        getKeyboard :: IO Keyboard
        getKeyboard = do
            -- check if the file exists first
            exists <- doesFileExist path

            -- if the file exists, load the keyboard layout stored in it
            if exists then do
                -- reading the file contents as bytestring
                fileContents <- BS.readFile path
        
                -- deserealising file contents to get load keyboard layout
                pure $ deserialiseLayout fileContents

            -- otherwise, just use the default keyboard layout
            else do
                op makeDefaultLayout


--- creates a list of all the keybinds
listOfKeybinds :: [KR.Keybind]
listOfKeybinds = [minBound..]


-- creates a list of all default inputs corresponding to every keybind
-- throws an error if there aren't as many inputs as binds
getDefaultInputs :: [SDL.Scancode]
getDefaultInputs = if length listOfKeybinds /= length KR.getDefaultInputsExposed then
                       error "Not as many default inputs as keybinds!"
                   else KR.getDefaultInputsExposed


-- loads the default keyboard layout
makeDefaultLayout :: Keyboard
makeDefaultLayout = Map.fromList $ zip listOfKeybinds getDefaultInputs


-- deserialises the json data into a keyboard layout
deserialiseLayout :: BS.ByteString -> Keyboard
deserialiseLayout jsonData = case Aeson.decodeStrict jsonData of
    -- if the conversion was successful, return the loaded layout
    Just layout -> layout

    -- otherwise, use the default layout
    Nothing     -> makeDefaultLayout


-- serialises the keyboard layout into json data
serialiseLayout :: Keyboard -> BS.ByteString
serialiseLayout = BS.toStrict . Aeson.encode


{-
-- initialise keypress corresponding to keyboard layout
-- essentially, if u wanna check if any key is being pressed, you can index it in the array
initialiseKeypressArray
-}

-- checks if a key has been pressed
genericKeypressChecker :: (MonadIO m)
                       => W.World
                       -> KR.Keybind
                       -> (Bool -> Bool -> Bool)
                       -> m Bool
-- fold over all provided events and see if one of them is the key that has been pressed
genericKeypressChecker w keybind f = pure $ f isKeyPressed wasKeyPressed where
        -- get a raw keybind (space, left, A, etc) corresponding to a keybind (jump, run, etc)
        keyraw   = fromJust $ keybind `Map.lookup` (W.getKb w)
    
        isKeyPressed  = (W.getKbs w)  keyraw
        wasKeyPressed = (W.getKbps w) keyraw

isKeyPressed :: (MonadIO m) => W.World -> KR.Keybind -> m Bool
isKeyPressed w keybind = genericKeypressChecker w keybind f where
    f x y = x && not y

isKeyHeld :: (MonadIO m) => W.World -> KR.Keybind -> m Bool
isKeyHeld w keybind = genericKeypressChecker w keybind f where
    f x y = x && y

isKeyDown :: (MonadIO m) => W.World -> KR.Keybind -> m Bool
isKeyDown w keybind = genericKeypressChecker w keybind f where
    f x y = x