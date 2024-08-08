import Test.Tasty
import Test.Tasty.HUnit

import SDLHelper.KeyboardReader as KB
import SDLHelper.Data.Rect      as R
import SDL

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
        -- testKeyboardReader,
        testRect
    ]

{--
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
    ]--}

testRect = testGroup "Rect" [
        testCase "Vertical overlap positive"   $ a `R.overlapsVertically` d @?= True,
        testCase "Vertical overlap negative"   $ a `R.overlapsVertically` c @?= False,
        testCase "Horizontal overlap positive" $ a `R.overlapsHorizontally` b @?= True,
        testCase "Horizontal overlap negative" $ a `R.overlapsHorizontally` d @?= False,
        testCase "Overlap positive"            $ a `R.overlaps` b @?= True,
        testCase "Overlap negative"            $ b `R.overlaps` d @?= False
    ] where
        a = R.Rect 0 0 10 10
        b = R.Rect 5 5 10 10
        c = R.Rect 15 15 10 10
        d = R.Rect 100 0 10 10

{--
runInputTest f keybind currentlyPressed r = SDL.getModState >>= \m -> (checkForInput f keybind currentlyPressed m) @?= r

checkForInput f keybind currentlyPressed m = f KB.makeDefaultLayout keybind $ map (\x -> makeKeyboardEvent x m) currentlyPressed

makeKeyboardEvent :: SDL.Keycode -> SDL.KeyModifier -> SDL.EventPayload
makeKeyboardEvent k m = SDL.KeyboardEvent $ SDL.KeyboardEventData Nothing SDL.Pressed False keysym where
        keysym = SDL.Keysym (SDL.Scancode 0) k m
--}
