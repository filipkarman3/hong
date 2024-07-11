module SDLHelper.SDLHelper where

import qualified SDL
import qualified SDL.Image

import SDLHelper.WorldExposed

import qualified SDLHelper.KeyboardReader as KB

import Control.Monad          (void, when)
import Control.Monad.Extra    (loopM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text              (Text)

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
     => (World -> [SDL.EventPayload] -> m World)
     -> World
     -> m World
loop op st = loopM (withEventHandling op) st

withEventHandling :: (MonadIO m)
                  => (World -> [SDL.EventPayload] -> m World)
                  -> World
                  -> m (Either World World)
withEventHandling op st = do
    -- get list of SDL events like keypresses
    events <- pollEvents

    -- quit the game if a quit event occurred
    if quitEventOccurred events then pure $ Right st

    --otherwise, run the game loop
    else do
        st' <- withTiming (withRendering op) st events
        pure $ Left st' -- return the game state

withTiming :: (MonadIO m) => (World -> [SDL.EventPayload] -> m World) -> World -> [SDL.EventPayload] -> m World
withTiming op st events = do
    -- get start time of tick
    starttick <- SDL.ticks

    -- perform game loop
    st' <- op st events

    -- get end time of tick
    endtick <- SDL.ticks

    -- keep the game running at specified fps if it's running too fast
    wait $ fromIntegral frameTime - (endtick - starttick)

    pure st'
    where
        frameTime :: Int
        frameTime = 1000 `div` fps st
        wait ms = when (20 > ms && ms > 0) $ SDL.delay ms

withRendering :: (MonadIO m) => (World -> [SDL.EventPayload] -> m World) -> World -> [SDL.EventPayload] -> m World
withRendering op st events = do
    -- clear the screen
    SDL.clear $ r st

    -- actually run the game tick
    st' <- op st events

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