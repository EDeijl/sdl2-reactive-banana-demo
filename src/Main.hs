{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Animations.Types
import           Animations.Utils
import           Control.Lens
import           Control.Monad
import           Data.Int
import           Foreign.C
import           Linear
import           Linear.Affine
import           Linear.V
import           Linear.V2
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           Reactive.Banana.SDL2
import           SDL                        hiding (Texture)
import           SDL.Image                  as Image


dt :: Double
dt = 20 * ms where ms = 1e-3

main :: IO ()
main = do
  sdlES <- getSDLEventSource
  graphicsData <- liftIO initGraphics
  network <- compile $ makeNetwork sdlES graphicsData
  actuate network
  runSDLPump sdlES

makeNetwork :: SDLEventSource -> GraphicsData -> MomentIO ()
makeNetwork sdlES gd = mdo
  etick <- tickEvent sdlES
  esdl  <- sdlEvent sdlES

  let emouse    = mouseEvent esdl
      speedup v = v ^* (vecLengthDouble v / 20)
      bvelocity :: Behavior (V2 Double)
      bvelocity = (\pos mouse -> speedup $ mouse ^-^ pos ^-^ V2 0 45) <$> bposition <*> bmouse
      bdraw     :: Behavior (IO ())
      bdraw     = renderRect' (renderer gd) <$> fmap round <$> fmap P bposition

  (bmouse :: Behavior (V2 Double)) <- fmap toDouble <$> fmap fromPoint
                                                    <$> stepper (P $ V2 0 0)
                                                                (filterJust $ justMotion <$> emouse)
  (bposition :: Behavior (V2 Double)) <- accumB (V2 0 0) $
                                           (\v pos -> (v ^* dt) ^+^ pos) <$> bvelocity <@ etick

  changes bdraw >>= reactimate'  -- reactimate on behaviors turned into events by the changes function
  reactimate $ render gd <$ etick

renderRect' :: Renderer -> Point V2 Int32 -> IO ()
renderRect' r (P (V2 x y)) = do
  rendererDrawColor r $= V4 minBound maxBound minBound maxBound
  drawRect r (Just (Rectangle (P (V2 (CInt x) (CInt y))) (V2 100 100)))

initGraphics :: IO GraphicsData
initGraphics = do
  SDL.initialize [InitVideo]
  Image.imgInit [InitPNG]
  w <- createWindow "Animations" defaultWindow
  r <- createRenderer w (-1) $ RendererConfig SoftwareRenderer False
  return $ GraphicsData w r



render :: GraphicsData -> IO ()
render gd = present (renderer gd)

justMotion :: EventPayload -> Maybe (Point V2 Int32)
justMotion (MouseMotionEvent (MouseMotionEventData _ _ _ pos _)) = Just pos
justMotion _ = Nothing

fromPoint :: Point V2 Int32 -> V2 Int32
fromPoint (P (V2 x y))= V2 x y

toDouble :: V2 Int32 -> V2 Double
toDouble v = V2 (fromIntegral $ v ^._x) (fromIntegral $ v ^._y)

vecLength :: (Floating a, Integral a1) => V2 a1 -> a
vecLength (V2 x y) = sqrt (fromIntegral (x*x + y*y))

vecLengthDouble :: V2 Double -> Double
vecLengthDouble (V2 x y) = sqrt (x*x + y*y)
