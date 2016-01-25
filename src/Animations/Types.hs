module Animations.Types where

import           Foreign.C
import           Linear.V2
import           SDL       hiding (Texture)
import qualified SDL       (Texture)

data Texture = Texture SDL.Texture (V2 CInt)

data GraphicsData = GraphicsData { window   :: Window
                                 , renderer :: Renderer
                                 }

