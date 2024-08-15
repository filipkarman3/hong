module SDLHelper.TextRenderer where

import qualified SDL
import qualified SDL.Font as SDLF

import qualified SDLHelper.SDLHelper as H

import qualified SDLHelper.Data.MiscData as MD (Sprite)

import qualified Data.Map as Map

import Control.Monad.IO.Class (MonadIO)

import Data.Text (Text)

withFont :: (MonadIO m) => FilePath -> Int -> (SDLF.Font -> m a) -> m a
withFont p pt f = do
    -- loads a font at a particular font size
    font <- SDLF.load p pt

    -- performs some computation utilising the font
    x <- f font

    -- frees the memory space used by the front
    SDLF.free font

    pure x

-- loads a font at some font size
-- creates several textures using that font
-- frees up space used by the font
-- returns the textures in the form of a map
loadSolidText :: (MonadIO m)
              => SDL.Renderer
              -> FilePath
              -> Int
              -> SDLF.Color
              -> [(String, Text)]
              -> m (Map.Map String MD.Sprite)
loadSolidText r p pt c ts = withFont p pt $ \font -> do
    -- creates a map (dictionary) containing strings and corresponding text
    -- note this text is in the form of a texture, you can render it to the screen
    l <- mapM (f font) ts

    -- returns the map
    pure $ Map.fromList l
    where
        -- generates a texture from some text
        f :: (MonadIO m) => SDLF.Font -> (String, Text) -> m (String, MD.Sprite)
        f font (str, text) = do
            -- create a surface with the text
            textS <- SDLF.solid font c text
            textT <- H.surfaceToTexture r textS
            pure (str, textT)

-- frees up space used by the loaded textures in the map
destroyTextMap :: (MonadIO m) => Map.Map String MD.Sprite -> m ()
destroyTextMap m = foldr (\x l -> l >> SDL.destroyTexture (fst $ snd x)) (pure ()) $ Map.toList m