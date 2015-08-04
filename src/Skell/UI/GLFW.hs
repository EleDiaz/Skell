module Skell.UI.GLFW
       (glfwFrontend
       )
       where

import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
-- import Control.Lens
import Control.Monad.State.Strict

import Data.Default
import qualified Data.Sequence as S
-- import qualified Data.Text as T

import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW
--import qualified Graphics.Rendering.FTGL   as FTGL

import Pipes
import Pipes.Lift

import System.Mem.Weak

import Skell.Types

fps :: Double
fps = 1/1

glfwFrontend :: Pipe ISkell OSkell IOSkell () -> IO ()
glfwFrontend coreModel = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  when r $ do
    m <- GLFW.createWindow 640 380 "Skell" Nothing Nothing
    case m of
      Just win -> do
        GLFW.makeContextCurrent m
        initGL win
        putStrLn "RunEffect"
        -- Not works correctly infinity loop into a thread. I don't know why.
        -- lay <- FTGL.createSimpleLayout
        --font <- FTGL.createTextureFont "NotoSans-Regular.ttf"
        -- measures <- FTGL.getFontBBox font "Háłö! hola"
        -- print measures
        -- addFinalizer lay (FTGL.destroyLayout lay)
        -- FTGL.setLayoutFont lay font
        --FTGL.setFontFaceSize font 72 72
        let font = ()
        runEffect $
                 (inputPipe font win
              >-> evalStateP def coreModel
              >-> drawPipe font win)
        putStrLn "unWait RunEffect"
        GLFW.destroyWindow win
      Nothing -> return ()
    GLFW.terminate
  where
    simpleErrorCallback e s =
      putStrLn $ unwords [show e, show s]


inputPipe :: () -> GLFW.Window -> Producer ISkell IO ()
inputPipe font win = do
  -- There are a dropCallback add feature to open files easy
  -- ScrollCallback
  oSkellVar <- lift $ atomically $ newTVar (Nothing::Maybe OSkell)
  -- key <- lift $ atomically $ newTVar (Nothing::Maybe key)
  charVar <- lift $ atomically $ newTVar (Nothing::Maybe Char)
  lift $ do
    -- CloseCallback is controlled by pipe else fail on close core dumped
    GLFW.setWindowCloseCallback win $ Just
      (\win' -> GLFW.destroyWindow win' >> GLFW.terminate)
    GLFW.setWindowRefreshCallback win (Just $ drawScene font)
    GLFW.setFramebufferSizeCallback win (Just resizeScene)
    GLFW.setCharCallback win $ Just
      (\_win char -> atomically $ writeTVar charVar (Just char))

  timeVar <- lift $ atomically $ newTVar 0.0
  let go = do
        lift $ do
          Just currTime <- GLFW.getTime
          lastTime <- atomically $ readTVar timeVar
          let gap = currTime - lastTime
          when (gap < fps) (threadDelay $ round (1000000 * (fps - gap)))
          Just currTime' <- GLFW.getTime
          atomically $ writeTVar timeVar currTime'

        mChar <- lift $ atomically $ readTVar charVar
        case mChar of
          Just char -> yield $ IKey (S.singleton $ KChar char)
          Nothing -> return ()
        lift $ atomically $ writeTVar charVar Nothing

        -- when (var == change?) (yield value)
        -- when (var2 == change?) (yield value)
        yield INone
        go
  go

drawPipe :: () -> GLFW.Window -> Consumer OSkell IO ()
drawPipe font win = do
  oSk <- await
  lift $ drawScene font win
  -- Esto es lo que impide que el bucle se pare para salir
  -- si le ponemos un condicionante detenemos el programa
  -- correctamente, y facilmente se pueden añadir dialogos
  -- de advertencia de salida
  drawPipe font win

initGL :: GLFW.Window -> IO ()
initGL win = do
  putStrLn "InitGL Things"
  GL.shadeModel $= GL.Smooth
  GL.clearColor $= GL.Color4 0 0 0 0
  GL.clearDepth $= 1
  GL.depthMask $= GL.Enabled
  GL.depthFunc $= Just GL.Lequal
  GL.lineSmooth $= GL.Enabled
  -- blend $= Enabled
  -- bxlendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  -- texture Texture2D $= Enabled
  (w,h) <- GLFW.getFramebufferSize win
  resizeScene win w h


resizeScene :: GLFW.FramebufferSizeCallback
resizeScene win w 0 = resizeScene win w 1
resizeScene _   w h = do
  GL.matrixMode $= GL.Projection
  GL.ortho 0 (fromIntegral w) 0 (fromIntegral h) (-1) 1
  --GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  GL.loadIdentity
  GL.flush

drawScene :: () -> GLFW.Window -> IO ()
drawScene font win = do
  GLFW.swapBuffers win
  GLFW.pollEvents
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  -- GL.currentColor $= GL.Color4 1 0 0 1
  -- GL.loadIdentity
  -- GL.translate $ GL.Vector3 (-1 :: GL.GLdouble) (-1) 0
  -- GL.scale (1/250 :: GL.GLdouble) (1/250) 1
  -- FTGL.renderFont font "Háłö! hola" FTGL.Side

  GL.currentColor $= GL.Color4 1 0 0 1
  GL.loadIdentity
  GL.renderPrimitive GL.Polygon $
    mapM_ (\(x,y,z) -> GL.vertex $ GL.Vertex3 x y z)
    [(0::GL.GLfloat, 0, 0.0)
    ,(0, 0.5, 0.0)
    ,(0.5, 0.5, 0.0)
    ,(0.5, 0, 0.0)]
  GL.flush

