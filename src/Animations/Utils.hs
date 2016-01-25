module Animations.Utils where

import           Animations.Types
import           Foreign.C
import           Linear
import           Linear.Affine
import           Paths_Animations (getDataFileName)
import           SDL              hiding (Texture)
import qualified SDL              (Texture)
import           SDL.Image


loadTexture :: Renderer -> FilePath -> IO Texture
loadTexture r filePath = do
    surface <- getDataFileName filePath >>= loadBMP
    size <- surfaceDimensions surface
    let key = V4 0 maxBound maxBound maxBound
    surfaceColorKey surface $= Just key
    t <- createTextureFromSurface r surface
    freeSurface surface
    return (Texture t size)


renderTexture :: Renderer -> Texture -> Maybe (Rectangle CInt) -> Point V2 CInt ->  IO ()
renderTexture r (Texture t size) clip xy =
  let dstSize = maybe size (\(Rectangle _ size') -> size') clip
  in copy r t clip (Just (Rectangle xy dstSize))

