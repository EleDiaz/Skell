module Skell.UI.Gtk
       (gtkFrontend
       )
       where

import           Control.Concurrent.Async (async, wait)
import           Control.Lens
import           Control.Monad.State.Strict

import           Data.Default
import qualified Data.Text                as T

import           Pipes
import           Pipes.Lift
import           Pipes.Concurrent

import           Graphics.Rendering.Cairo hiding (x, y)
import           Graphics.UI.Gtk          as GTK

import           Skell.Types
import           Skell.UI.Gtk.Keys

gtkFrontend :: Pipe ISkell OSkell IOSkell () -> IO ()
gtkFrontend coreModel = do
    void $ initGUI
    win <- windowNew
    vBox <- vBoxNew False 2
    infoBarW <- labelNew $ Just "Label"
    GTK.set win [ containerChild := vBox ]

    (consumer, producer) <- makeLayoutText vBox win

    a <- async $ runEffect $ producer >-> evalStateP def coreModel >-> consumer

    boxPackStart vBox infoBarW PackNatural 0

    void $ GTK.on win deleteEvent $ do
        liftIO mainQuit
        return False
    widgetShowAll win
    mainGUI
    wait a
    return ()

-- onLines :: (String -> IO a) -> IO b
-- onLines callback = forever $ do
--     str <- getLine
--     callback str


makeLayoutText :: MonadIO m
                  => VBox
                  -> Window
                  -> IO (Consumer OSkell m (), Producer ISkell m ())
makeLayoutText vBox win = do
    area <- drawingAreaNew
    GTK.set area [ widgetCanFocus := True]
    boxPackStart vBox area PackGrow 5

    ctxt <- cairoCreateContext Nothing
    lay <- layoutEmpty ctxt
    layoutSetWrap lay WrapWholeWords

    -- Wrap the layout to a different width each time the window is resized.
    void $ GTK.on area sizeAllocate $ \(Rectangle _ _ w _) ->
        layoutSetWidth lay (Just (fromIntegral w))
    -- Setup the handler to draw the layout.
    void $ GTK.on area draw $ updateLayout' lay

    -- Set up input method
    im <- imMulticontextNew

    (output, input) <- spawn $ newest 1  -- ISkell
    (output2, input2) <- spawn $ newest 1 -- OSkell

    -- Añadir el resaltado desde aqui!!! El View (String, [Atrs])
    let relayout = do
          oSk <- await
          lift $ postGUISync $ do
            layoutSetText lay (oSk^.textDisplayO)
            layoutSetAttributes lay []
            widgetQueueDraw area
          relayout


    void $ GTK.on im imContextCommit $ \str -> do
        _ <- atomically $ send output $ convertKeys (Nothing, Just str)
        return ()

    void $ GTK.on area realize $ imContextSetClientWindow im =<< widgetGetWindow win
    void $ GTK.on area focusInEvent  $ liftIO (imContextFocusIn  im) >> return True
    void $ GTK.on area focusOutEvent $ liftIO (imContextFocusOut im) >> return True
    void $ GTK.on area keyReleaseEvent $ imContextFilterKeypress im
    void $ GTK.on area keyPressEvent $ do
        imHandled <- imContextFilterKeypress im
        if imHandled then return True else do
            modifiers <- eventModifier
            when (modifiers /= []) $ do
                kName <- eventKeyName
                kChar <- fmap keyToChar eventKeyVal
                _ <- liftIO $ atomically $ send output $ convertKeys (Just $ T.unpack kName, fmap (:[]) kChar)
                return ()
            return True

    void $ forkIO $ runEffect (fromInput input2 >-> relayout) -- binding
    atomically $ send output def -- first Refresh screen
    return (toOutput output2, fromInput input)

updateLayout' :: PangoLayout -> Render ()
updateLayout' lay = do
    moveTo 5 5
    (PangoRectangle x y _ h,_) <- liftIO $ layoutGetCursorPos lay 0 -- TODO: indicar el cursor
    setSourceRGB 0 0 0
    setLineWidth 2
    moveTo x y
    lineTo x (y+h)
    stroke
    showLayout lay

