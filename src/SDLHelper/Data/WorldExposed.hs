module SDLHelper.Data.WorldExposed where

import qualified SDL
import qualified SDL.Font as SDLF
import qualified SDL.Mixer as SDLM

import qualified SDLHelper.Data.Keyboard as KB
import qualified SDLHelper.Data.MiscData as MD

import qualified Player as P (Player)
import qualified Ball   as B (Ball)
import qualified Bot    as Bot (Bot)

import qualified Data.Map as Map

import Control.Monad.IO.Class (MonadIO)

data Scene = Menu | Game deriving (Show, Eq)

data World = World {
    wr     :: WorldRaw,
    player :: P.Player,
    ball   :: B.Ball,
    score  :: (Int, Int),
    bot    :: Bot.Bot,
    text   :: Map.Map String MD.Drawthing,
    scene  :: Scene,
    played :: Bool,
    sounds :: [SDLM.Chunk],
    soundQ :: [SDLM.Chunk]
}

addSound :: World -> SDLM.Chunk -> World
addSound w s = w { soundQ = s : soundQ w }

playSounds :: (MonadIO m) => World -> m World
playSounds w = foldr (\x l -> l >> SDLM.play x) (pure ()) (soundQ w)
    >> pure (w { soundQ = [] })

data WorldRaw = WorldRaw {
    kb   :: KB.Keyboard,
    kbs  :: SDL.Scancode -> Bool,
    kbps :: SDL.Scancode -> Bool,
    w    :: SDL.Window,
    es   :: [SDL.EventPayload],
    r    :: SDL.Renderer,
    fps  :: Int,
    quit :: Bool,
    logger :: [String]
}

log :: World -> String -> World
log w s = w { wr = wr' } where
    wr1 = wr w
    wr' = wr1 { logger = s : logger wr1 }

clearLog :: World -> World
clearLog w = w { wr = wr' } where
    wr1 = wr w
    wr' = wr1 { logger = [] }

getKb   w = kb $ wr w
getKbs  w = kbs $ wr w
getKbps w = kbps $ wr w
getW    world = w $ wr world
getEs   w = es $ wr w
getR    w = r $ wr w
getFps  w = fps $ wr w
getQuit w = quit $ wr w

setKb w x   = w { wr = wr' } where wr' = (wr w) { kb   = x }
setKbs w x  = w { wr = wr' } where wr' = (wr w) { kbs  = x }
setKbps w x = w { wr = wr' } where wr' = (wr w) { kbps = x }
setW w x    = w { wr = wr' } where wr' = (wr w) { w    = x }
setEs w x   = w { wr = wr' } where wr' = (wr w) { es   = x }
setR w x    = w { wr = wr' } where wr' = (wr w) { r    = x }
setFps w x  = w { wr = wr' } where wr' = (wr w) { fps  = x }
setQuit w x = w { wr = wr' } where wr' = (wr w) { quit = x }

changePlayerX :: World -> Float -> World
changePlayerX w n = w { player = p' } where
    p = player w
    p' = MD.changeX p n

changePlayerY :: World -> Float -> World
changePlayerY w n = w { player = p' } where
    p = player w
    p' = MD.changeY p n