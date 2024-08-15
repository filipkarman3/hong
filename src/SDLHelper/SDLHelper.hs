{-# LANGUAGE OverloadedStrings #-}

module SDLHelper.SDLHelper where

import qualified SDL
import qualified SDL.Image

import SDLHelper.Data.WorldExposed (World(wr), WorldRaw(..), getKb, logger, clearLog)
import SDLHelper.Data.Rect

import qualified SDLHelper.Data.MiscData  as MD

import qualified SDLHelper.KeyboardReader as KB
import qualified SDLHelper.Data.Keyboard  as KB (Keyboard)

import qualified SDL.Font as SDLF

import Control.Monad          (void, when)
import Control.Monad.Extra    (loopM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text              (Text)

doMain :: Text
       -> (Int, Int)
       -> FilePath
       -> (WorldRaw -> IO World)
       -> (World -> IO World)
       -> (World -> IO ())
       -> IO ()
doMain winName (winX, winY) kbPath fInit fLoop fTerminate = withSDL
    $ withWindow winName (winX, winY)
    $ \w -> withRenderer w
    $ \r -> KB.withKeyboard kbPath
    $ \kb -> do
        -- this isn't necessary right now (ie: we don't need to know the kbstate rn)
        -- BUT the kbstate is a parameter of WorldRaw, so we define it here as a dummy variable
        -- it is used to initialise WorldRaw and will get updated on each consecutive frame
        kbState <- SDL.getKeyboardState

        -- SDL.Font needs to be initialised
        SDLF.initialize

        -- define a lot of base datatypes here to prevent the library user having to it themselves
        let raw = WorldRaw {
            kb   = kb,
            kbs  = kbState,
            kbps = kbState,
            w    = w,
            es   = [],
            r    = r,
            fps  = 50,
            quit = False,
            logger = []
        }

        world <- fInit raw
        world' <- loop fLoop world
        fTerminate world'

        -- gotta unload some SDL.Font stuff
        SDLF.quit

        -- gotta return the keyboard layout
        pure $ getUpdatedKb world'
    
    where

        getUpdatedKb :: World -> KB.Keyboard
        getUpdatedKb world = getKb world

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
    
    let raw = wr st

    -- quit the game if a quit event occurred
    if quitEventOccurred events || quit raw then pure $ Right st

    --otherwise, run the game loop
    else do
        -- update the necessary values, like keypresses done this turn
        let raw' = raw { es = events, kbps = (kbs raw), kbs = state }

        -- then actuall run the frame
        st' <- withTiming (withRendering op) (st { wr = raw' })
        
        -- return the game state
        pure $ Left st'

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
        frameTime = 1000 `div` (fps $ wr st)
        wait ms = when (20 > ms && ms > 0) $ SDL.delay ms

withRendering :: (MonadIO m) => (World -> m World) -> World -> m World
withRendering op st = do
    -- clear the screen
    SDL.clear $ (r $ wr st)

    -- actually run the game tick
    st' <- op st >>= outputLogs

    -- render changes to the screen
    SDL.present $ (r $ wr st')

    pure st'

outputLogs :: (MonadIO m) => World -> m World
outputLogs w = do
    foldr (\s l -> (liftIO . print) s >> l) (pure ()) (logger $ wr w)
    pure $ clearLog w

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

-- given a surface, it creates a texture and frees space used up by the surface
surfaceToTexture :: (MonadIO m) => SDL.Renderer -> SDL.Surface -> m MD.Sprite
surfaceToTexture r s = do
    -- create a texture from the surface
    t <- SDL.createTextureFromSurface r s
    i <- SDL.queryTexture t

    -- free up memory taken up by surface
    SDL.freeSurface s

    -- return the texture
    pure (t, i)
