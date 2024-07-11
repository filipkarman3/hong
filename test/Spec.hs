import Test.Tasty
import Test.Tasty.HUnit

import KeyboardReader as KB
import SDL
    
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
        testKeyboardReader
    ]

testKeyboardReader = testGroup "KeyboardReader" [
        testCase "Up press registered successfully"               $ runInputTest KB.isKeyPressed KB.Up [SDL.KeycodeUp] True,
        testCase "Up hold returns false when not holding up"      $ runInputTest KB.isKeyHeld    KB.Up [SDL.KeycodeUp] False,
        testCase "iskeydown succeeds when up pressed"             $ runInputTest KB.isKeyDown    KB.Up [SDL.KeycodeUp] True,
        testCase "Up press not registered when only holding A"    $ runInputTest KB.isKeyPressed KB.Up [SDL.KeycodeA]  False,
        testCase "Down press not registered when holding nothing" $ runInputTest KB.isKeyPressed KB.Up []              False,
        testCase "Up hold returns true when holding up"           $ SDL.getModState >>= \m
                                                                    -> KB.isKeyDown KB.makeDefaultLayout KB.Up [
                                                                           SDL.KeyboardEvent $ SDL.KeyboardEventData Nothing SDL.Pressed True (SDL.Keysym (SDL.Scancode 0) SDL.KeycodeUp m)
                                                                       ] @?= True
    ]

runInputTest f keybind currentlyPressed r = SDL.getModState >>= \m -> (checkForInput f keybind currentlyPressed m) @?= r

checkForInput f keybind currentlyPressed m = f KB.makeDefaultLayout keybind $ map (\x -> makeKeyboardEvent x m) currentlyPressed

makeKeyboardEvent :: SDL.Keycode -> SDL.KeyModifier -> SDL.EventPayload
makeKeyboardEvent k m = SDL.KeyboardEvent $ SDL.KeyboardEventData Nothing SDL.Pressed False keysym where
        keysym = SDL.Keysym (SDL.Scancode 0) k m