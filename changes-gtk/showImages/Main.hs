module Main
    ( main
    ) where

import Changes.Core
import Changes.UI.GTK
import Data.Media.Image
import Shapes
import System.Directory
import System.FilePath

imagedir :: FilePath
imagedir = "changes-gtk/showImages/images"

main :: IO ()
main = do
    filenames <- listDirectory imagedir
    --(paths, double, selTest, saveOpt) <- O.execParser (O.info optParser mempty)
    changesMainGTK $ \tc -> do
        let newWindow spec = ccExitOnClosed tc $ createWindow spec
        -- fileReference :: FilePath -> Reference ByteStringEdit
        imageRef <- liftIO $ makeMemoryReference (blankImage @PixelRGB8 (100, 100) black) $ \_ -> True
        model <- liftLifeCycle $ makeReflectingModel imageRef
        rec
            (_, closer) <-
                lifeCycleEarlyCloser $
                newWindow $ let
                    wsPosition = WindowPositionCenter
                    wsSize = (600, 600)
                    wsCloseBoxAction :: View ()
                    wsCloseBoxAction = liftIO closer
                    wsTitle :: Model (ROWUpdate Text)
                    wsTitle = constantModel "Images"
                    setFileRef :: FilePath -> MenuEntry
                    setFileRef filename =
                        ActionMenuEntry (pack filename) Nothing $
                        constantModel $
                        Just $ do
                            bs <- liftIO $ readFile $ imagedir </> filename
                            image <-
                                resultTextToM $ do
                                    (_, jpegImage) <- decode (jpegFormat 50) bs
                                    decode jpegImageTrue8 jpegImage
                            liftIO $
                                runResource emptyResourceContext model $ \asub -> do
                                    _ <- pushEdit noEditSource $ aModelEdit asub $ pure $ MkWholeReaderEdit image
                                    return ()
                    wsMenuBar :: Maybe (Model (ROWUpdate MenuBar))
                    wsMenuBar = Just $ constantModel $ pure $ SubMenuEntry "Image" $ fmap setFileRef filenames
                    wsContent :: CreateView Widget
                    wsContent = createImage $ mapModel toReadOnlyChangeLens model
                    in MkWindowSpec {..}
        return ()
