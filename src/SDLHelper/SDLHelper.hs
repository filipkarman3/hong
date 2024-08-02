{-# LANGUAGE OverloadedStrings #-}

module SDLHelper.SDLHelper where

import qualified SDL
import qualified SDL.Image

import SDLHelper.Data.WorldExposed (World(..))
import SDLHelper.Data.Rect

import qualified SDLHelper.KeyboardReader as KB
import qualified SDLHelper.Data.Keyboard  as KB (Keyboard)

import Control.Monad          (void, when)
import Control.Monad.Extra    (loopM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text              (Text)

doMain :: Text
       -> (Int, Int)
       -> FilePath
       -> (KB.Keyboard -> SDL.Window -> SDL.Renderer -> IO World)
       -> (World -> IO World)
       -> (World -> IO ())
       -> IO ()
doMain winName (winX, winY) kbPath fInit fLoop fTerminate = withSDL
    $ withWindow winName (winX, winY)
    $ \w -> withRenderer w
    $ \r -> KB.withKeyboard kbPath
    $ \kb -> do
        world <- fInit kb w r
        world' <- loop fLoop world
        fTerminate world'

        -- gotta return the keyboard layout
        pure $ getUpdatedKb world'
    
    where

        getUpdatedKb :: World -> KB.Keyboard
        getUpdatedKb world = kb world

withSDL :: (MonadIO m) => m a -> m ()
withSDL op = do
    SDL.initializeAll
    _ <- op
    SDL.quit

withWindow :: (MonadIO m) => Text -> (Int, Int) -> (SDL.Window -> m a) -> m ()
withWindow title (x, y) op = do
    w <- createSizedWindow title (x, y)
    SDL.showWindow w
    void $ op w
    SDL.destroyWindow w

createSizedWindow :: (MonadIO m) => Text -> (Int, Int) -> m SDL.Window
createSizedWindow title (x, y) = SDL.createWindow title settings where
    settings   = SDL.defaultWindow { SDL.windowInitialSize = resolution }
    resolution = SDL.V2 (fromIntegral x) (fromIntegral y)

withRenderer :: (MonadIO m) => SDL.Window -> (SDL.Renderer -> m a) -> m ()
withRenderer w op = do
    SDL.HintRenderVSync SDL.$= SDL.EnableVSync
    r <- SDL.createRenderer w (-1) rendererConfig
    op r
    SDL.destroyRenderer r

rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig {
        SDL.rendererType = SDL.SoftwareRenderer,
        SDL.rendererTargetTexture = False
    }

-- iteratively poll for SDL events, perform some operation, quit upon a QuitEvent
loop :: (MonadIO m)
     => (World -> m World)
     -> World
     -> m World
loop op st = loopM (withEventHandling op) st

withEventHandling :: (MonadIO m)
                  => (World -> m World)
                  -> World
                  -> m (Either World World)
withEventHandling op st = do
    -- get list of SDL events like keypresses
    events <- pollEvents
    state  <- SDL.getKeyboardState
    

    -- quit the game if a quit event occurred
    if quitEventOccurred events || quit st then pure $ Right st

    --otherwise, run the game loop
    else do
        st' <- withTiming (withRendering op) (st { es = events, kbps = (kbs st), kbs = state } )
        pure $ Left st' -- return the game state

withTiming :: (MonadIO m) => (World -> m World) -> World -> m World
withTiming op st = do
    -- get start time of tick
    starttick <- SDL.ticks

    -- perform game loop
    st' <- op st

    -- get end time of tick
    endtick <- SDL.ticks

    -- keep the game running at specified fps if it's running too fast
    wait $ fromIntegral frameTime - (endtick - starttick)

    pure st'
    where
        frameTime :: Int
        frameTime = 1000 `div` fps st
        wait ms = when (20 > ms && ms > 0) $ SDL.delay ms

withRendering :: (MonadIO m) => (World -> m World) -> World -> m World
withRendering op st = do
    -- clear the screen
    SDL.clear $ r st

    -- actually run the game tick
    st' <- op st

    -- render changes to the screen
    SDL.present $ r st'

    pure st'


-- get all the events that have happened since the last time this function was called
pollEvents :: (MonadIO m) => m [SDL.EventPayload]
pollEvents = map SDL.eventPayload <$> SDL.pollEvents

-- check for a quit event
quitEventOccurred :: [SDL.EventPayload] -> Bool
quitEventOccurred es = SDL.QuitEvent `elem` es

loadTexture :: (MonadIO m) => SDL.Renderer -> FilePath -> m (SDL.Texture, SDL.TextureInfo)
loadTexture r p = do
  t <- SDL.Image.loadTexture r p
  i <- SDL.queryTexture t
  pure (t, i)


-- centers a rect on another rect
centerRect :: Rect -> Rect -> Rect
centerRect a b = a { rectX = newX, rectY = newY } where
    centerX = (toRational . rectX) b + (toRational . rectW) b /2
    centerY = (toRational . rectY) b + (toRational . rectH) b /2
    newX    = toInt $ centerX - (toRational . rectW) a /2
    newY    = toInt $ centerY - (toRational . rectH) a /2

    toInt :: (RealFrac a) => a -> Int
    toInt = round

toSDLRect :: a -> a -> a -> a -> SDL.Rectangle a
toSDLRect a b c d = SDL.Rectangle e f where
    e = SDL.P $ SDL.V2 a b
    f = SDL.V2 c d