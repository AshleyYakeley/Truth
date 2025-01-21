module Main
    ( main
    )
where

import Changes.Core
import Data.Media.Image
import Shapes
import System.Directory
import System.FilePath

import Changes.World.GNOME.GTK

imagedir :: FilePath
imagedir = "Changes/changes-gnome/examples/showImages/images"

resultTextToM :: MonadFail m => Result Text a -> m a
resultTextToM = resultToM . mapResultFailure unpack

main :: IO ()
main = do
    filenames <- listDirectory imagedir
    -- (paths, double, selTest, saveOpt) <- O.execParser (O.info optParser mempty)
    runLifecycle
        $ runGTK
        $ \gtkContext ->
            runView
                $ runGView gtkContext
                $ do
                    -- fileReference :: FilePath -> Reference ByteStringEdit
                    imageRef <-
                        gvLiftIONoUI
                            $ makeMemoryReference (Just $ MkSomeFor NoAlphaTrue8PixelType $ blankImage (100, 100) black)
                            $ \_ ->
                                True
                    model <- gvLiftLifecycle $ makeReflectingModel imageRef
                    rec (_, closer) <-
                            gvGetState
                                $ createWindow
                                $ let
                                    wsPosition = WindowPositionCenter
                                    wsSize = (600, 600)
                                    wsCloseBoxAction :: GView 'Locked ()
                                    wsCloseBoxAction = gvRunUnlocked $ gvCloseState closer
                                    wsTitle :: Model (ROWUpdate Text)
                                    wsTitle = constantModel "Images"
                                    setFileRef :: FilePath -> MenuEntry
                                    setFileRef filename =
                                        ActionMenuEntry (constantModel (pack filename, Nothing))
                                            $ constantModel
                                            $ Just
                                            $ do
                                                bs <- liftIO $ readFile $ imagedir </> filename
                                                image <-
                                                    resultTextToM $ do
                                                        (_, jpegImage) <- decode (jpegFormat 50) bs
                                                        decode jpegImageTrue8 jpegImage
                                                liftIO
                                                    $ runResource emptyResourceContext model
                                                    $ \asub -> do
                                                        _ <-
                                                            pushEdit noEditSource
                                                                $ aModelEdit asub
                                                                $ pure
                                                                $ MkWholeReaderEdit
                                                                $ Just
                                                                $ MkSomeFor NoAlphaTrue8PixelType image
                                                        return ()
                                    wsContent :: AccelGroup -> GView 'Unlocked Widget
                                    wsContent ag = do
                                        mb <- createMenuBar ag $ pure $ SubMenuEntry "Image" $ fmap setFileRef filenames
                                        uic <- createImage $ mapModel toReadOnlyChangeLens model
                                        createLayout
                                            OrientationVertical
                                            [(defaultLayoutOptions, mb), (defaultLayoutOptions{loGrow = True}, uic)]
                                    in MkWindowSpec{..}
                    return ()
