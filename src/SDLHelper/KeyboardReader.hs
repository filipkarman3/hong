{-# LANGUAGE TypeSynonymInstances #-}

module SDLHelper.KeyboardReader where

import SDLHelper.KeyboardReaderExposed (getDefaultInputsExposed, Keybind(..))

import qualified SDL


import System.Directory (doesFileExist)

import qualified Data.ByteString      as BS  (readFile, writeFile, ByteString, toStrict)
import qualified Data.Map.Strict      as Map
import qualified Data.Aeson           as Aeson

import Data.Maybe (fromJust, isNothing)


type Keyboard = Map.Map Keybind KeycodeWrapper

-- needed for encoding of Keybinds to work
instance Aeson.ToJSON Keybind where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON Keybind

-- these two are needed because map keys are encoded differently I think?
instance Aeson.ToJSONKey Keybind

instance Aeson.FromJSONKey Keybind


-- this type synonym has been defined because I don't want to directly interact with and make extensions to the SDL.Keycode class
type KeycodeWrapper = SDL.Keycode

-- needed for encoding and decoding of SDL keypresses to work
instance Aeson.ToJSON KeycodeWrapper where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON KeycodeWrapper


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
listOfKeybinds :: [Keybind]
listOfKeybinds = [minBound..]


-- creates a list of all default inputs corresponding to every keybind
-- throws an error if there aren't as many inputs as binds
getDefaultInputs :: [SDL.Keycode]
getDefaultInputs = if length listOfKeybinds /= length getDefaultInputsExposed then
                       error "Not as many default inputs as keybinds!"
                   else getDefaultInputsExposed



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
genericKeypressChecker :: Keyboard
                       -> Keybind
                       -> [SDL.EventPayload]
                       -> (SDL.Keycode -> SDL.EventPayload -> Bool)
                       -> Bool
-- fold over all provided events and see if one of them is the key that has been pressed
genericKeypressChecker kb keybind es f = case keyraw of
    Just key -> foldr (\x l -> f key x || l) False es
    Nothing  -> False
    where

        -- get a raw keybind (space, left, A, etc) corresponding to a keybind (jump, run, etc)
        -- returns nothing if the key was not found to have a corresponding keybind
        -- in this case, the key is ignored
        keyraw   = keybind `Map.lookup` kb

evaluateKeypress :: SDL.Keycode -> SDL.InputMotion -> Maybe Bool -> SDL.EventPayload -> Bool
evaluateKeypress targetKeycode targetMotion targetRepeat event =
    if isNothing keyData'
        then False
    else
        matchingKeycode && matchingMotion && matchingRepeat
    where
        compare :: (Eq a) => Maybe a -> a -> Bool
        compare x y     = case x of
            Nothing -> True
            Just x' -> x' == y

        matchingKeycode = targetKeycode == keyKeycode
        keyKeycode      = SDL.keysymKeycode keyKeysym
        keyKeysym       = SDL.keyboardEventKeysym keyData

        matchingMotion  = targetMotion == keyMotion
        keyMotion       = SDL.keyboardEventKeyMotion keyData

        matchingRepeat  = compare targetRepeat keyRepeat
        keyRepeat       = SDL.keyboardEventRepeat keyData

        keyData :: SDL.KeyboardEventData
        keyData = fromJust $ keyData'

        keyData' :: Maybe SDL.KeyboardEventData
        keyData' = convertToData event

        convertToData :: SDL.EventPayload -> Maybe SDL.KeyboardEventData
        convertToData (SDL.KeyboardEvent k) = Just k
        convertToData _                     = Nothing


isKeyPressed :: Keyboard -> Keybind -> [SDL.EventPayload] -> Bool
isKeyPressed kb keybind es = genericKeypressChecker kb keybind es f where
    f x y = evaluateKeypress x SDL.Pressed (Just False) y

isKeyHeld :: Keyboard -> Keybind -> [SDL.EventPayload] -> Bool
isKeyHeld kb keybind es = genericKeypressChecker kb keybind es f where
    f x y = evaluateKeypress x SDL.Pressed (Just True) y

isKeyDown :: Keyboard -> Keybind -> [SDL.EventPayload] -> Bool
isKeyDown kb keybind es = genericKeypressChecker kb keybind es f where
    f x y = evaluateKeypress x SDL.Pressed Nothing y